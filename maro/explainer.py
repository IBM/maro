from .util import convert_class_name, split_step_param, get_param_schema
from .remediator import Compare, constraint_formatting
import copy
import lale.operators

def _describe_constraint(c, planned, addon=False):

    description = "Unknown constraint"

    step, con = constraint_formatting(c)

    if not step and not con:
        return "ERROR: Step or constraint not found!"

    if 'hyperparameter#' in c:
        hp = c['hyperparameter#']
        step,param = split_step_param(hp)
        cls_step = convert_class_name(step)
    else:
        return "ERROR: No hyperparameter in constraint!"

    if cls_step == "Top":
        # special case of single operators
        step = planned
        # special, special case of single operator being a choice
        if isinstance(planned, lale.operators.OperatorChoice):
            for step2 in planned.steps_list():
                param_schema = get_param_schema(step2, param)
                if param_schema:
                    step = step2
        cls_step = convert_class_name(step.name())
        #print(cls_step)


    positive_param = "Try setting argument '{}' in operator {}".format(param, cls_step)
    negative_param = "Avoid setting argument '{}' in operator {}".format(param, cls_step)
    remove_param = "Try ensuring that argument '{}' in operator {} is present for all runs\n\t(a Choice operator may need to be removed)".format(param, cls_step)

    if not con["properties"]["absent"]:
        # check for special case of not absent and operator in question is in a choice
        # if so, remove choice
        # special, special case of single operator which is a choice
        if isinstance(planned, lale.operators.OperatorChoice):
            for step2 in planned.steps_list():
                if step2.name() == cls_step:
                    description = remove_param
                    return description
        else:
            if isinstance(planned, lale.operators.BasePipeline):
                for step2 in planned.steps_list():
                    if isinstance(step2, lale.operators.OperatorChoice):
                        for step3 in step2.steps_list():
                            if step3.name() == cls_step:
                                description = remove_param
                                return description

    value = con[param]

    if con["properties"]["present"]:
        description = remove_param
    elif con["properties"]["discrete"]:
        if value is not None:
            if not con["properties"]["negated"]:
                if value == "!0!" or value == "!1!":
                    # special case, don't allow this value, probably looking for default
                    if con["properties"]["absent"]:
                        description = "{} to the default value".format(positive_param)
                else:
                    description = "{} to '{}'".format(positive_param, value)
                    if con["properties"]["absent"]:
                        description += " or the default value"
            else:
                if value == "!0!" or value == "!1!":
                    # special case, don't allow this value, probably looking for default
                    if not con["properties"]["absent"]:
                        description = "{} to the default value".format(negative_param)
                else:
                    description = "{} to '{}'".format(negative_param, value)
                    if not con["properties"]["absent"]:
                        description += " nor the default value"
    else:
        if "compare" in con["properties"] and con["properties"]["compare"]:
            compare = con["properties"]["compare"]
            if "compare_to_param" in con["properties"] and con["properties"]["compare_to_param"]:
                if con["properties"]["compare_to_param"] == "!0!":
                    # special case of nonsense hyperparameter name
                    pass
                else:
                    other_step,other_param = split_step_param(con["properties"]["compare_to_param"])
                    other_cls_step = convert_class_name(other_step)
                    value = "parameter '{}' in operator {}".format(other_param, other_cls_step)

            if compare == Compare.GT:
                description = "{} to a value greater than {}".format(positive_param, value)
            elif compare == Compare.GTE:
                description = "{} to a value greater than or equal to {}".format(positive_param, value)
            elif compare == Compare.LT:
                description = "{} to a value less than {}".format(positive_param, value)
            elif compare == Compare.LTE:
                description = "{} to a value less than or equal to {}".format(positive_param, value)
            if con["properties"]["absent"]:
                description += " or the default value"

    return description

def explain(solver_resp, planned):
    """ Given solver response, convert into natural language for user. """

    if len(solver_resp) == 0:
        return "Empty constraints!"

    if len(solver_resp) == 1:
        # Single constraint case
        resp_constraints = solver_resp[0]

        if not isinstance(resp_constraints, list):
            resp_constraints = [ resp_constraints ]

        desc = ""
        for c in resp_constraints:
            desc_con = _describe_constraint(c, planned)
            if len(desc) == 0:
                desc = desc_con
            else:
                desc += "\nAND\n{}".format(desc_con)
    else:
        # Tree of constraints
        # NOTE: currently assuming simple tree with only one left and right
        root_c = solver_resp[0]
        left_c = solver_resp[1][0]
        right_c = solver_resp[1][1]

        rootn_c = copy.deepcopy(root_c)
        if "negated?#" not in rootn_c:
            rootn_c["negated?#"] = True
        else:
            rootn_c["negated?#"] = (not rootn_c["negated?#"])
        if "absent-ok?#" in rootn_c:
            rootn_c["absent-ok?#"] = (not rootn_c["absent-ok?#"])

        root_desc = _describe_constraint(root_c, planned)
        left_desc = None
        if not isinstance(left_c, bool) or left_c != True:
            left_desc = _describe_constraint(left_c, planned)
            left_desc = left_desc[0].lower() + left_desc[1:]
        right_desc = None
        if not isinstance(right_c, bool) or right_c != True:
            right_desc = _describe_constraint(right_c, planned)
            right_desc = right_desc[0].lower() + right_desc[1:]

        rootn_desc = _describe_constraint(rootn_c, planned)

        if left_desc:
            left_side_desc = "{} and {}".format(root_desc, left_desc)
        else:
            left_side_desc = root_desc
        if right_desc:
            right_side_desc = "{} and {}".format(rootn_desc, right_desc)
        else:
            right_side_desc = rootn_desc

        desc = "{}\nOR\n{}".format(left_side_desc, right_side_desc)

    return desc
