import numbers
import math
import difflib
from IPython.display import display, Markdown
from lale.operators import BasePipeline, OperatorChoice
from lale.json_operator import from_json
import copy
import pprint, json
import jsonschema
import re
import numpy as np
import requests
import json
import datetime
from .util import convert_class_name, identify_step, get_param_schema, camelCase_to_snake
from .remediator import remediate
from .explainer import explain

def auto_remediate(planned, hyperopt_trained, explanation=False):
    """ Convienence method that takes planned pipeline and automl results
        and calls solver and remediator.
        Optionally explains in console the fix. """
    fs = for_solver(hyperopt_trained)
    resp = call_solver(fs, planned)
    r_json = resp.json()

    if explanation:
        print(explain(r_json, planned))

    return remediate(r_json, planned)

def no_nan(x):
    if isinstance(x, numbers.Number) and math.isnan(x):
        return None
    else:
        return x

def pipeline_defaults(pipeline):
    if isinstance(pipeline, BasePipeline):
        return { p.class_name(): { k: no_nan(v) for (k, v) in p.get_defaults().items() } for p in pipeline.steps_list() }
    else:
        return { pipeline.class_name(): { k: no_nan(v) for (k, v) in pipeline.get_defaults().items() } }

def pipeline_schemas(pipeline):
    if isinstance(pipeline, BasePipeline):
        return { s.class_name(): s.hyperparam_schema() for s in pipeline.steps_list() }
    else:
        return { pipeline.class_name(): pipeline.hyperparam_schema() }

def for_solver(hyperopt_trained):
    flags = [ name for name in hyperopt_trained.summary() ]
    all_pipelines = {name: hyperopt_trained.get_pipeline(name)
                 for name in hyperopt_trained.summary().index}
    return [ { "pipeline_id": name,
               "pipeline_definition": pipeline.to_json(),
               "pipeline_schemas": pipeline_schemas(pipeline),
               "pipeline_defaults": pipeline_defaults(pipeline),
               "result": { p: no_nan(hyperopt_trained.summary().at[name, p]) for p in flags }
              } for name, pipeline in all_pipelines.items()]

def myconverter(obj):
    try:
        if math.isnan(obj):
            return Null
    except:
        pass

    if obj is None:
        return Null
    elif isinstance(obj, np.integer):
        return int(obj)
    elif isinstance(obj, np.floating):
        return float(obj)
    elif isinstance(obj, np.ndarray):
        return obj.tolist()
    elif isinstance(obj, datetime.datetime):
        return obj.__str__()

def call_solver(for_solver, planned):
    fs = json.dumps(for_solver, default=myconverter)
    #print(fs)
    pp = json.dumps(planned.to_json())
    #print(pp)
    out = { 'pipelines': fs, 'planned': pp }
    #pprint.pprint(out)

    return requests.post('http://localhost:8000/pipelines', data=out)

def pipeline_diff(first_pl, second_pl):
    first_str = first_pl.pretty_print(customize_schema=True, show_imports=False)
    first_lines = first_str.splitlines()

    second_str = second_pl.pretty_print(customize_schema=True, show_imports=False)
    second_lines = second_str.splitlines()

    differ = difflib.Differ()
    compare = differ.compare(first_lines, second_lines)
    display(Markdown("```diff\n{}\n```".format("\n".join(compare))))

def replace_step(fail_case, org_step_name, new_step_name, planned):
    # Replace step with newer step from planned pipeline
    f_json = copy.deepcopy(fail_case)
    p_json = planned.to_json()


    #print("**planned pipeline:**\n")
    #pprint.pprint(p_json)
    #print("**original pipeline:**\n")
    #pprint.pprint(f_json)


    # go through edges and replace
    new_edges = []
    for edge in f_json["edges"]:
        if org_step_name in edge:
            new_edge = edge
            new_edge[new_edge.index(org_step_name)] = new_step_name
            new_edges.append(new_edge)
        else:
            new_edges.append(edge)
    f_json["edges"] = new_edges

    # go through steps and replace with version in planned pipeline
    # NOTE: not going through choices recursively which would be better, assuming 1 level atm
    new_steps = {}
    for op_name,op_json in f_json["steps"].items():
        if op_name == org_step_name:
            for step_name,step_json in p_json["steps"].items():
                if step_name == new_step_name:
                    new_steps[new_step_name] = step_json
                elif step_name == "choice":
                    choice = p_json["steps"]["choice"]
                    for c_name,c_step in choice["steps"].items():
                        if c_name == new_step_name:
                            if c_step["state"] == "planned":
                                if "is_frozen_trainable" not in c_step:
                                    c_step["is_frozen_trainable"] = True
                                if "customize_schema" not in c_step:
                                    c_step["customize_schema"] = {}
                                if "hyperparams" not in c_step:
                                    c_step["hyperparams"] = {}
                                c_step["state"] = "trainable"
                            new_steps[new_step_name] = c_step
        else:
            new_steps[op_name] = op_json
    f_json["steps"] = new_steps

    #print("**modified pipeline:**\n")
    #pprint.pprint(f_json)

    return f_json

def evaluate_pipeline(pl_json, train_X, train_y):
    try:
        pl = from_json(pl_json)
    except jsonschema.ValidationError:
        # NOTE: single parameter removal does not work, guess that's a failure?
        return False
    except KeyError:
        # NOTE: this happens due to edge error, not sure why
        return False
    except RecursionError:
        # NOTE: not sure why this happens, I count this as also a validation Error
        return False
    except TypeError:
        # NOTE: note sure why
        return False

    try:
        pl.fit(train_X, train_y)
        return True
    except:
        return False

def _run_shortcut(f_case, g_case, planned, succeed, train_X, train_y, verbose=False):
    #g_pl = from_json(g_case["pipeline_definition"])
    #f_pl = from_json(f_case["pipeline_definition"])

    # maintain lists of parameters changed
    f_case_params = {}
    current_params = {}

    if verbose:
        print("Failing case chosen:\n")
        pprint.pprint(f_case)
        print("\n")
        print("Successful case chosen:\n")
        pprint.pprint(g_case)

    # modification: switch steps first, then parameters starting from first step

    # look for steps that are different, if found, replace with version from planned pipeline
    # also will just assume pipelines are linear and same size for now

    # this happens if just one operator, for simplicity, create steps
    if "steps" not in f_case["pipeline_definition"]:
        f_case = copy.deepcopy(f_case)
        step_name = re.sub(r'(?<!^)(?=[A-Z])', '_', f_case["pipeline_definition"]["label"]).lower()
        f_case["pipeline_definition"]["steps"] = {step_name: f_case["pipeline_definition"]}
        f_case["pipeline_definition"]["edges"] = []
    if "steps" not in g_case["pipeline_definition"]:
        g_case = copy.deepcopy(g_case)
        step_name = re.sub(r'(?<!^)(?=[A-Z])', '_', g_case["pipeline_definition"]["label"]).lower()
        g_case["pipeline_definition"]["steps"] = {step_name: g_case["pipeline_definition"]}
        g_case["pipeline_definition"]["edges"] = []

    current_json = f_case["pipeline_definition"]

    if f_case["pipeline_definition"]["steps"].keys() != g_case["pipeline_definition"]["steps"].keys():

        for step_index in range(len(f_case["pipeline_definition"]["steps"].keys())):

            #working_json = copy.deepcopy(current_json)

            current_f_step = list(f_case["pipeline_definition"]["steps"].keys())[step_index]
            current_g_step = list(g_case["pipeline_definition"]["steps"].keys())[step_index]
            if current_f_step != current_g_step:
                working_pl_json = replace_step(f_case["pipeline_definition"], current_f_step, current_g_step, planned)
                #pprint.pprint(current_pl.to_json())
                # on failure, update current_pl (which should have the step change)
                f_case_params["step_{}".format(step_index)] = {current_f_step: ""}
                current_params["step_{}".format(step_index)] = {current_f_step: ""}
                if not evaluate_pipeline(working_pl_json, train_X, train_y):
                    current_json = working_pl_json
                    current_params["step_{}".format(step_index)] = {current_g_step: ""}
                    if verbose:
                        print("step {}: {} to {} failed, update!".format(step_index, current_f_step, current_g_step))

    # now, go through each step's parameters, if the step doesn't exist in the pipeline, ignore it
    for step_name,step_op in g_case["pipeline_definition"]["steps"].items():

        working_json = copy.deepcopy(current_json)

        # need to cover case where step replaces existing one
        if step_name not in working_json["steps"]:

            for step_index in range(len(g_case["pipeline_definition"]["steps"].keys())):
                if list(g_case["pipeline_definition"]["steps"].keys())[step_index] == step_name:
                    try:
                        replacement_step = list(working_json["steps"].keys())[step_index]
                    except IndexError as e:
                        # NOTE: case of unevenly sized pipelines, go ahead and skip
                        continue
                    working_json = replace_step(working_json, replacement_step, step_name, planned)

        if step_name not in working_json["steps"]:
            continue

        # go through each param in the step
        if "hyperparams" in step_op and step_op["hyperparams"]:

            for param_name,param_value in step_op["hyperparams"].items():
                #print(param_name, param_value)

                org_param_value = "default"

                if "hyperparams" not in working_json["steps"][step_name]:
                    working_json["steps"][step_name]["hyperparams"] = {}
                if param_name in working_json["steps"][step_name]["hyperparams"]:
                    org_param_value = working_json["steps"][step_name]["hyperparams"][param_name]
                    # same param and value, skip
                    if org_param_value == param_value:
                        continue

                if step_name not in current_params:
                    current_params[step_name] = {}
                current_params[step_name][param_name] = org_param_value

                if step_name in f_case["pipeline_definition"]["steps"]:
                    if step_name not in f_case_params:
                        f_case_params[step_name] = {}
                    f_case_params[step_name][param_name] = org_param_value

                if verbose:
                    print("testing: {}: {}={}".format(step_name, param_name, param_value))

                working_json["steps"][step_name]["hyperparams"][param_name] = param_value
                #pprint.pprint(working_json)

                if not evaluate_pipeline(working_json, train_X, train_y):
                    current_json = working_json
                    current_params[step_name][param_name] = param_value

                    if verbose:
                        print("{}: {}={} fails! Update!".format(step_name, param_name, param_value))

        # cover cases where hyperparameter exists in failing case but not successful case (i.e. defaults)
        if "hyperparams" in working_json["steps"][step_name]:

            for param_name,param_value in working_json["steps"][step_name]["hyperparams"].items():
                if "hyperparams" in step_op and param_name in step_op["hyperparams"]:
                    continue

                if step_name not in current_params:
                    current_params[step_name] = {}
                current_params[step_name][param_name] = param_value

                if step_name in f_case["pipeline_definition"]["steps"]:
                    if step_name not in f_case_params:
                        f_case_params[step_name] = {}
                    f_case_params[step_name][param_name] = param_value

                # remove param (set to default)
                mod_json = copy.deepcopy(working_json)
                del mod_json["steps"][step_name]["hyperparams"][param_name]

                if verbose:
                    print("testing: {}: {}={}".format(step_name, param_name, "default"))

                if not evaluate_pipeline(mod_json, train_X, train_y):
                    current_json = mod_json

                    current_params[step_name][param_name] = "default"

                    if verbose:
                        print("{}: {}={} fails! Update!".format(step_name, param_name, "default"))

    # now go through params of each and identify intersection
    faults = {}
    for step_name,param_dict in current_params.items():
        if step_name in f_case_params:
            f_param_dict = f_case_params[step_name]
            for param_name,param_value in param_dict.items():
                if param_name in f_param_dict and f_param_dict[param_name] == param_value:
                    if step_name not in faults:
                        faults[step_name] = {}
                    faults[step_name][param_name] = param_value

    if verbose:
        print("faults found before checking:")
        pprint.pprint(faults)

    # now go through and double-check faults
    for g_case in succeed:

        if "steps" not in g_case["pipeline_definition"]:
            g_case = copy.deepcopy(g_case)
            step_name = re.sub(r'(?<!^)(?=[A-Z])', '_', g_case["pipeline_definition"]["label"]).lower()
            g_case["pipeline_definition"]["steps"] = {step_name: g_case["pipeline_definition"]}
            g_case["pipeline_definition"]["edges"] = []

        g_json = g_case["pipeline_definition"]
        for step_name,param_dict in faults.items():
            # check if steps exist
            if step_name.startswith("step_"):
                for s in param_dict.keys():
                    if s in g_json["steps"]:
                        if verbose:
                            print("{} found in good case, abort!".format(s))
                        return {"{} found in good case, abort!".format(s):""}
            else:
                # check each param in the step
                if step_name in g_json["steps"]:
                    if param_name in g_json["steps"][step_name] and g_json["steps"][step_name][param_name] == param_value:
                        if verbose:
                            print("{}:{}_{} found in good case, abort!".format(step_name, param_name, param_value))
                        return {"{}:{}_{} found in good case, abort!".format(step_name, param_name, param_value):""}

    #if verbose:
    #        pipelineDiff(f_pl, current_pl)

    return faults

def _get_default_value(op, param, planned):
    # Given param, find what the default value is supposed to be
    value = None
    step = identify_step(op, planned)
    schema = get_param_schema(step, param)
    if "default" in schema:
        value = schema["default"]
    return value


def _convert_constraints(faults, planned, negate=True, verbose=False):
    # if verbose:
    #     print(faults)
    #print(faults)
    param_constraints = []

    if len(faults.keys()) == 0:
        return None
    if list(faults.keys())[0].endswith("abort!"):
        return list(faults.keys())[0]

    for op,con in faults.items():

        if op.startswith("step_"):
            # special case of removing an operator
            s = op.split("_")
            step_num = int(s[1])
            # NOTE: we're expecting the op at the step_num to be a choice
            # we'll be lazy and just find the first other op in the choice
            if isinstance(planned, OperatorChoice):
                # special case of single operator, still assume is choice
                step = planned
            else:
                step = planned.steps_list()[step_num]
            other_op = None
            before_ops = set()
            after_ops = set()
            for before,after in con.items():
                before_ops.add(before)
                if len(after) > 0:
                    other_op = after
                    break
            if not other_op:
                for step2 in step.steps_list():
                    snake_step = camelCase_to_snake(step2.name())
                    if snake_step not in before_ops:
                        other_op = snake_step
                        break
            if not other_op:
                raise Exception("Operator to remove has no alternative!")
            hyperparam = "{}.{}".format(other_op,"replace")
            pc = {
                "hyperparameter#": hyperparam,
                'parameter-present#': True,
                'absent-ok?#': False
            }
            param_constraints.append(pc)
        else:
            for param,value in con.items():
                hyperparam = "{}.{}".format(op,param)
                if value == "default":
                    value = _get_default_value(op, param, planned)

                pc = {
                    "hyperparameter#": hyperparam,
                    "negated?#": True,
                    "absent-ok?#": True
                }
                if not negate:
                    pc["negated?#"] = False
                    pc["absent-ok?#"] = False
                param_constraints.append(pc)
                if type(value) == bool:
                    pc["boolean-value#"] = value
                elif type(value) == str:
                    pc["string-value#"] = value
                elif type(value) == int:
                    pc["numeric-value#"] = value
                elif type(value) == float:
                    pc["numeric-value#"] = value
                #print(type(value))
                #print("{} = {}".format(hyperparam, value))

    constraints = [param_constraints]
    if verbose:
        print(constraints)
    return constraints

def shortcut(planned, forsolver, train_X, train_y, verbose=False):
    failed = list(filter(lambda x: x["result"]["status"] == "fail", forsolver))
    succeed = list(filter(lambda x: x["result"]["status"] == "ok", forsolver))
    # modification: shortcut prefers disjoint but that concept doesn't really make sense here, we'll just pick the first one
    # seems like shortcut algo only needs one case of each
    f_case = failed[0]
    g_case = succeed[0]

    faults = _run_shortcut(f_case, g_case, planned, succeed, train_X, train_y, verbose)
    return _convert_constraints(faults, planned, negate=True, verbose=verbose)

def stacked_shortcut(planned, forsolver, train_X, train_y, verbose=False):
    failed = list(filter(lambda x: x["result"]["status"] == "fail", forsolver))
    succeed = list(filter(lambda x: x["result"]["status"] == "ok", forsolver))
    # still seems like stacked shortcut algo only needs one case of each
    f_case = failed[0]

    faults = {}

    for g_case in succeed:
        g_case_faults = _run_shortcut(f_case, g_case, planned, succeed, train_X, train_y, verbose)

        # don't allow abort messages in union
        cleaned_g_case_faults = {}
        for k,v in g_case_faults.items():
            if "found in good case, abort!" in k:
                continue
            cleaned_g_case_faults[k] = v

        faults.update(cleaned_g_case_faults)

    return _convert_constraints(faults, planned, negate=True, verbose=verbose)

def defaultcut(planned, forsolver, train_X, train_y, verbose=False):
    # basic algorithm: try each parameter default individually until it works,
    #   then try operators (based on successful cases)
    #   then try 2-gram parameters
    failed = list(filter(lambda x: x["result"]["status"] == "fail", forsolver))
    succeed = list(filter(lambda x: x["result"]["status"] == "ok", forsolver))
    # NOTE: pick the first failing case here to focus on
    f_case = failed[0]
    #g_case = succeed[0]

    # this happens if just one operator, for simplicity, create steps
    if "steps" not in f_case["pipeline_definition"]:
        f_case = copy.deepcopy(f_case)
        step_name = re.sub(r'(?<!^)(?=[A-Z])', '_', f_case["pipeline_definition"]["label"]).lower()
        f_case["pipeline_definition"]["steps"] = {step_name: f_case["pipeline_definition"]}
        f_case["pipeline_definition"]["edges"] = []

    #g_pl = from_json(g_case["pipeline_definition"])
    #f_pl = from_json(f_case["pipeline_definition"])
    #current_pl = from_json(f_case["pipeline_definition"])

    # maintain lists of parameters changed
    #f_case_params = {}
    #current_params = {}

    if verbose:
        print("Failing case chosen:\n")
        pprint.pprint(f_case)
        #print("\n")
        #print("Successful case chosen:\n")
        #pprint.pprint(g_case)

    # first, iterate through params (1-gram) and try defaults

    # now, go through each step's parameters and set to default, see if it works
    for step_name,step_op in f_case["pipeline_definition"]["steps"].items():

        # cover cases where hyperparameter exists in failing case but not successful case (i.e. defaults)
        if "hyperparams" in f_case["pipeline_definition"]["steps"][step_name]:

            for param_name,param_value in f_case["pipeline_definition"]["steps"][step_name]["hyperparams"].items():

                # set parameter to default via deleting it
                working_json = copy.deepcopy(f_case["pipeline_definition"])
                del working_json["steps"][step_name]["hyperparams"][param_name]

                if verbose:
                    #pprint.pprint(working_json)
                    print("testing: {}: {}={}".format(step_name, param_name, "default"))

                if evaluate_pipeline(working_json, train_X, train_y):
                    fault = {step_name: {param_name: "default"}}
                    return _convert_constraints(fault, planned, negate=False, verbose=verbose)
                else:
                    if verbose:
                        print("{}: {}={} fails!".format(step_name, param_name, "default"))

    # now try replacing operators by comparing successful cases to failing case
    operator_changes = []
    for g_case in succeed:

        if "steps" not in g_case["pipeline_definition"]:
            g_case = copy.deepcopy(g_case)
            step_name = re.sub(r'(?<!^)(?=[A-Z])', '_', g_case["pipeline_definition"]["label"]).lower()
            g_case["pipeline_definition"]["steps"] = {step_name: g_case["pipeline_definition"]}
            g_case["pipeline_definition"]["edges"] = []

        if f_case["pipeline_definition"]["steps"].keys() != g_case["pipeline_definition"].keys():

            op_change = {}
            for step_index in range(len(f_case["pipeline_definition"]["steps"].keys())):

                current_f_step = list(f_case["pipeline_definition"]["steps"].keys())[step_index]
                current_g_step = list(g_case["pipeline_definition"]["steps"].keys())[step_index]
                if current_f_step != current_g_step:

                    op_change["step_{}".format(step_index)] = (current_f_step, current_g_step)

            if op_change not in operator_changes:
                operator_changes.append(op_change)

    if verbose:
        pprint.pprint(operator_changes)

    for op_change in operator_changes:
        for step_name, steps in op_change.items():
            before_step = steps[0]
            after_step = steps[1]
            if verbose:
                print("testing {}: {} => {}".format(step_name, before_step, after_step))
            working_pl_json = replace_step(f_case["pipeline_definition"], before_step, after_step, planned)
            if evaluate_pipeline(working_pl_json, train_X, train_y):
                fault = {step_name: {before_step: after_step}}
                return _convert_constraints(fault, planned, negate=False, verbose=verbose)
            else:
                if verbose:
                    print("{}: {} => {} fails!".format(step_name, before_step, after_step))

    # now, go through each 2-gram parameters and set both default, see if it works
    for step_name,step_op in f_case["pipeline_definition"]["steps"].items():

        # cover cases where hyperparameter exists in failing case but not successful case (i.e. defaults)
        if "hyperparams" in f_case["pipeline_definition"]["steps"][step_name]:

            for param_name,param_value in f_case["pipeline_definition"]["steps"][step_name]["hyperparams"].items():

                for step_name2,step_op2 in f_case["pipeline_definition"]["steps"].items():

                    if "hyperparams" in f_case["pipeline_definition"]["steps"][step_name2]:

                        for param_name2,param_value2 in f_case["pipeline_definition"]["steps"][step_name2]["hyperparams"].items():

                            if step_name == step_name2 and param_name == param_name2:
                                continue

                            # set parameters to default via deleting it
                            working_json = copy.deepcopy(f_case["pipeline_definition"])
                            del working_json["steps"][step_name]["hyperparams"][param_name]
                            del working_json["steps"][step_name2]["hyperparams"][param_name2]

                            if verbose:
                                #pprint.pprint(working_json)
                                print("testing: {}: {}={}\n\t{}: {}={}".format(step_name, param_name, "default",step_name2, param_name2, "default"))

                            if evaluate_pipeline(working_json, train_X, train_y):
                                fault = {step_name: {param_name: "default"}}
                                return _convert_constraints(fault, planned, negate=False, verbose=verbose)
                            else:
                                if verbose:
                                    print("failed: {}: {}={}\n\t{}: {}={}".format(step_name, param_name, "default",step_name2, param_name2, "default"))

    if verbose:
        print("Did not find solution")
    return {}
