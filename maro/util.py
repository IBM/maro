import re, copy
import lale.operators

def convert_class_name(s):
    # special case with unconstrained__
    s = s.replace("__", "_")

    s2 = re.sub(r'(?!^)_([a-zA-Z])', lambda m: m.group(1).upper(), s)
    s2 = s2[0].upper() + s2[1:]

    if "pca" in s2 or "Pca" in s2:
        # special case of acronyms
        s2 = s2.replace("pca", "PCA")
        s2 = s2.replace("Pca", "PCA")
    if "Unconstrained" in s2:
        # special case where underscore was inserted
        s2 = s2.replace("Unconstrained", "Unconstrained_")
    return s2

def split_step_param(parameter_string):
    # Given something like op_name.param_name, separates the two out
    step = parameter_string[:parameter_string.index(".")]
    param = parameter_string[parameter_string.index(".")+1:]
    return step,param

def get_param_schema(op, param):
    # Given op and param, get matching hyperparameter schema for given param
    # Returns None if param not found
    schema = op.hyperparam_schema()
    if "allOf" in schema:
        # NOTE: for now, assume there's just one?
        if param not in schema["allOf"][0]["properties"]:
            return None
        return copy.deepcopy(schema["allOf"][0]["properties"][param])

def identify_step(step_name, planned):
    # Given step_name (in underscores), return matching op in planned
    # NOTE: just returns the first that works, assumes a single match
    # NOTE: only looks through choices for now and nested pipelines but only one level
    #print(step_name)
    if step_name == "top":
        # special case of single operator case
        return planned

    cls_step = convert_class_name(step_name)
    for step in planned.steps_list():
        if cls_step == step.name():
            return step
        elif isinstance(step, lale.operators.OperatorChoice):
            for step2 in step.steps_list():
                if cls_step == step2.name():
                    return step2
    return None

def camelCase_to_snake(name):
    # taken from lale
    s1 = re.sub("(.)([A-Z][a-z]+)", r"\1_\2", name)
    return re.sub("([a-z0-9])([A-Z])", r"\1_\2", s1).lower()
