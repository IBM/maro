import pprint
from lale.json_operator import from_json
import lale.operators
from .util import convert_class_name, split_step_param, get_param_schema, identify_step
from enum import Enum
import copy
import warnings

# Arbitrary number of bins to split a parameter into when doing between-parameter comparisons
PARAMETER_BINS = 5

class Compare(Enum):
    GT = "GT"
    GTE = "GTE"
    LT = "LT"
    LTE = "LTE"
    EQ = "EQ"
    NEQ = "NEQ"

def constraint_formatting(resp_c):
    # Reformats constraint into key, value form for a dict
    if 'hyperparameter#' in resp_c:
        hp = resp_c['hyperparameter#']
        step,param = split_step_param(hp)

    discrete = True
    value = None
    if 'boolean-value#' in resp_c:
        value = resp_c['boolean-value#']
    elif 'string-value#' in resp_c:
        # string takes precedence
        value = resp_c['string-value#']
    elif 'numeric-value#' in resp_c:
        discrete = False
        value = resp_c['numeric-value#']

    absent = False
    if 'absent-ok?#' in resp_c and resp_c['absent-ok?#']:
        absent = resp_c['absent-ok?#']

    negated = False
    if 'negated?#' in resp_c and resp_c['negated?#']:
        negated = True
        # special case for boolean values
        if 'boolean-value#' in resp_c:
            value = (not resp_c['boolean-value#'])
            negated = False

    present = False
    if 'parameter-present#' in resp_c and resp_c['parameter-present#']:
        present = True

    compare = None
    if 'is-less-equal#' in resp_c and resp_c['is-less-equal#']:
        discrete = False
        if negated:
            compare = Compare.GT
        else:
            compare = Compare.LTE
    if 'is-less-than#' in resp_c and resp_c['is-less-than#']:
        discrete = False
        if negated:
            compare = Compare.GTE
        else:
            compare = Compare.LT
    elif "is-less-equal#" in resp_c and 'is-less-than#' in resp_c:
        if not resp_c['is-less-equal#'] and not resp_c['is-less-than#']:
            discrete = True
            if negated:
                compare = Compare.NEQ
            else:
                compare = Compare.EQ

    compare_param = None
    if 'compare-hyperparameter#' in resp_c and resp_c['compare-hyperparameter#']:
        discrete = False
        if 'other-hyperparameter#' in resp_c:
            compare_param = resp_c['other-hyperparameter#']
            if compare_param == "!0!":
                # special case, basically just ignore this
                compare_param = None


    if not step or not param:
        return None, None

    constraint = {
        "properties": {}
    }
    if value is not None:
        constraint[param] = value
    else:
        constraint[param] = "parameter-present"
    constraint["properties"]["negated"] = negated
    constraint["properties"]["discrete"] = discrete
    constraint["properties"]["absent"] = absent
    constraint["properties"]["present"] = present
    if compare:
        constraint["properties"]["compare"] = compare
    if compare_param:
        constraint["properties"]["compare_to_param"] = compare_param

    return step, constraint

def _remove_choice(op_to_keep, planned, replace_op=None):
    if isinstance(planned, lale.operators.OperatorChoice):
        # special case where planned is just a single choice
        return op_to_keep
    for step in planned.steps_list():
        if isinstance(step, lale.operators.OperatorChoice):
            if op_to_keep in step.steps_list():
                # found the choice with the op
                old_op = step
                new_op = op_to_keep
                if replace_op:
                    new_op = replace_op
                return planned.replace(old_op, new_op)

def _bin_parameter(param_schema, bins=PARAMETER_BINS):
    # NOTE: Assuming for now that minimum and maximum exist
    binned_schemas = []
    param_min = param_schema["minimum"]
    param_max = param_schema["maximum"]
    bin_range = (param_max - param_min) / bins
    bin_range = int(bin_range)
    index_value = param_min
    for i in range(bins):
        bin_schema = copy.deepcopy(param_schema)
        bin_schema["minimum"] = index_value
        bin_schema["exclusiveMinimum"] = True
        if i == 0:
            # special case, match original schema for exclusiveMinimum
            if "exclusiveMinimum" not in param_schema or param_schema["exclusiveMinimum"] == False:
                bin_schema["exclusiveMinimum"] = False
        bin_schema["maximum"] = index_value + bin_range
        index_value += bin_range
        if bin_schema["maximum"] > param_max:
            bin_schema["maximum"] = param_max
        if i == bins - 1:
            # special case, match original schema for exclusiveMaximum
            if "exclusiveMaximum" in param_schema and param_schema["exclusiveMaximum"]:
                bin_schema["exclusiveMaximum"] = True
        binned_schemas.append(bin_schema)
    return binned_schemas

def _create_comparison_schema(param, param_schema, value, compare):
    schema_arg = {
        param: param_schema
    }
    if compare == Compare.GT:
        schema_arg[param]["minimum"] = value
        schema_arg[param]["exclusiveMinimum"] = True
    elif compare == Compare.GTE:
        schema_arg[param]["minimum"] = value
        schema_arg[pparam]["exclusiveMinimum"] = False
    elif compare == Compare.LT:
        schema_arg[param]["maximum"] = value
        schema_arg[param]["exclusiveMaximum"] = True
    elif compare == Compare.LTE:
        schema_arg[param]["maximum"] = value
        schema_arg[param]["exclusiveMaximum"] = False
    return schema_arg

def _update_pipeline(constraints, planned):
    # NOTE: currently assuming that we're just modifying constraints
    #new_plan = planned_json.copy()
    #print("before")
    #pprint.pprint(planned.to_json())

    new_plan = planned

    for step,c in constraints.items():

        op_new = identify_step(step, planned)
        if not op_new:
            raise Exception("Operator {} not found!".format(step))

        for p,v in c.items():
            # reserved words
            if p == "properties":
                continue
            # actual parameters
            if step == "top":
                # special case of single-operator, have to manually look for param and op
                if isinstance(planned, lale.operators.OperatorChoice):
                    for step2 in planned.steps_list():
                        param_schema = get_param_schema(step2, p)
                        if param_schema:
                            op_new = step2

            param_schema = get_param_schema(op_new, p)

            if c["properties"]["present"]:
                # Parameter must exist case
                # This case removes a choice in favor for the op with the parameter
                new_planned = _remove_choice(op_new, planned)
                return new_planned
            elif c["properties"]["discrete"]:
                # Discrete value case
                if not c["properties"]["negated"]:
                    if v == "!0!" or v == "!1!":
                        # Special case, impossible value
                        allowed = []
                    else:
                        allowed = [ v ]
                    if c["properties"]["absent"]:
                        # Default value is okay in this case, add to allowed list
                        if "default" in param_schema:
                            allowed.append(param_schema["default"])

                    schema_arg = {
                        p: {
                            "enum": allowed
                        }
                    }
                    op_new = op_new.customize_schema(**schema_arg)
                else:
                    # negation case
                    if v == "!0!" or v == "!1!":
                        # Special case, impossible value
                        not_allowed = []
                    else:
                        not_allowed = [v]
                    if not c["properties"]["absent"]:
                        # Default value is not okay, add to not allowed list
                        if "default" in param_schema:
                            not_allowed.append(param_schema["default"])

                    schema_arg = {
                        p: {
                            "allOf": [
                                param_schema,
                                {"not": {"enum": not_allowed}}
                            ]
                        }
                    }
                    op_new = op_new.customize_schema(**schema_arg)
            else:
                # Non-discrete (numeric) cases
                if "compare_to_param" in c["properties"]:
                    if "compare" not in c["properties"]:
                        #raise Exception("Must have type of comparison when comparing to hyperparameter!")
                        warnings.warn("Missing type of comparison when comparing to hyperparameter, ignoring!")
                    else:
                        # compare to other hyperparameter (can be in other operator)
                        other = c["properties"]["compare_to_param"]
                        other_step,other_param = split_step_param(other)
                        other_op = identify_step(other_step, planned)
                        other_schema = get_param_schema(other_op, other_param)
                        # split other parameter into bins
                        other_schemas = _bin_parameter(other_schema)
                        # now create matching schemas that match comparison
                        new_pipelines = []
                        for o_schema in other_schemas:
                            compare = c["properties"]["compare"]
                            if compare == Compare.GT or compare == Compare.GTE:
                                value = o_schema["maximum"]
                            else:
                                value = o_schema["minimum"]
                            param_schema = get_param_schema(op_new, p)
                            schema_arg = _create_comparison_schema(p, param_schema, value, compare)
                            if schema_arg[p]["maximum"] == schema_arg[p]["minimum"]:
                                # special case, ignore these schemas since they're meaningless
                                continue
                            other_schema_arg = {
                                other_param: o_schema
                            }
                            new_other = other_op.customize_schema(**other_schema_arg)
                            new_op = op_new.customize_schema(**schema_arg)
                            new_pl = planned.replace(other_op, new_other)
                            new_pl = new_pl.replace(op_new, new_op)
                            new_pipelines.append(new_pl)
                        return lale.operators.make_choice(*new_pipelines)
                elif "compare" in c["properties"]:
                    compare = c["properties"]["compare"]
                    schema_arg = _create_comparison_schema(p, param_schema, v, compare)
                    op_new = op_new.customize_schema(**schema_arg)

        if not isinstance(planned, lale.operators.BasePipeline):
            # special case of a single operator
            if not c["properties"]["absent"] and isinstance(planned, lale.operators.OperatorChoice):
                # special, special case of single operator being a choice
                return _remove_choice(op_new, planned)
            return op_new

        op_old = identify_step(step, planned)

        if not c["properties"]["absent"]:
            # check for special case of parameter being in a choice
            # if cannot be absent, remove choice
            for step in planned.steps_list():
                if isinstance(step, lale.operators.OperatorChoice) and op_old in step.steps_list():
                    new_planned = _remove_choice(op_old, planned, op_new)
                    return new_planned

        new_plan = planned.replace(op_old, op_new)

    return new_plan

def remediate(solver_resp, planned, verbose=False):
    """ Given a response from the solver and the original planned pipeline,
        return a remediated version of the planned pipeline. """

    if verbose:
        print(solver_resp)
        pprint.pprint(planned.to_json())

    p_json = planned.to_json()

    if len(solver_resp) == 0:
        return None

    if len(solver_resp) == 1:
        # Single constraint case
        resp_constraints = solver_resp[0]

        constraints = []
        if not isinstance(resp_constraints, list):
            resp_constraints = [ resp_constraints ]
        for c in resp_constraints:
            step, constraint = constraint_formatting(c)
            constraint = { step: constraint }
            constraints.append(constraint)

        if verbose:
            pprint.pprint(constraints)

        new_plan = planned
        for c in constraints:
            new_plan = _update_pipeline(c, new_plan)

        if verbose:
            print("after")
            pprint.pprint(new_plan.to_json())

        return new_plan
    else:
        # Tree of constraints
        # NOTE: currently assuming simple tree with only one left and right
        root_c = solver_resp[0]
        left_c = solver_resp[1][0]
        right_c = solver_resp[1][1]
        root_step,root_constraint = constraint_formatting(root_c)
        rootc = {root_step: root_constraint}
        if not isinstance(left_c, bool):
            left_step,left_constraint = constraint_formatting(left_c)
            lc = {left_step: left_constraint}
        if not isinstance(right_c, bool):
            right_step,right_constraint = constraint_formatting(right_c)
            rc = {right_step: right_constraint}
        rootn_c = copy.deepcopy(root_c)
        if "negated?#" not in rootn_c:
            rootn_c["negated?#"] = True
        else:
            rootn_c["negated?#"] = (not rootn_c["negated?#"])
        if "absent-ok?#" in rootn_c:
            rootn_c["absent-ok?#"] = (not rootn_c["absent-ok?#"])

        rootn_step,rootn_constraint = constraint_formatting(rootn_c)
        rootnc = {rootn_step: rootn_constraint}
        # do each side seperately and then combine
        left_pl = _update_pipeline(rootc, planned)
        # if side is just True, then skip
        if not isinstance(left_c, bool) or left_c != True:
            left_pl = _update_pipeline(lc, left_pl)
        if verbose:
            print("left")
            pprint.pprint(left_pl.to_json())
        right_pl = _update_pipeline(rootnc, planned)
        # if side is just True, then skip
        if not isinstance(right_c, bool) or right_c != True:
            right_pl = _update_pipeline(rc, right_pl)
        if verbose:
            print("right")
            pprint.pprint(right_pl.to_json())
        new_pl = lale.operators.make_choice(left_pl, right_pl)

        if verbose:
            print("after")
            pprint.pprint(new_pl.to_json())

        return new_pl
