import csv
from pathlib import Path
from collections import defaultdict
from itertools import chain, combinations, product
import re
import pandas as pd
from alignment_references import *
from slugify import slugify

# language-level questions / parameters
CASE_P = "flagging"
INDEX_P = "indexing"
TAM_P = "tam-split"
SPLIT_S_P = "split-s"
FEAT_P = "indexing-features"
DOM_P = "dom"
DOM_T_P = "dom-trigger"
REF_N_FREQ_P = "align-noun"
REF_P_FREQ_P = "align-pro"
REF_I_FREQ_P = "align-index"
REF_N_NL_FREQ_P = "align-noun-nl"
REF_P_NL_FREQ_P = "align-pro-nl"
REF_I_NL_FREQ_P = "align-index-nl"
REF_ERG_P = "ref-ergative"
REF_ERG_LOC_P = "ref-ergative-domain"
REF_TRI_P = "ref-tripartite"
REF_TRI_LOC_P = "ref-tripartite-domain"
REF_HOR_P = "ref-horizontal"
REF_HOR_LOC_P = "ref-horizontal-domain"
REF_SENS_P = "ref-sensitive"
REF_SENS_LOC_P = "ref-sensitive-domain"
COARG_SENS_F_P = "coargument-sensitivity flagging"
COARG_SENS_I_P = "coargument-sensitivity indexing"


# keywords for the language-level attribute-value dictionary
CODER = "Coder"
SOURCE = "Source"
VALUE = "Value"
ALIGNMENT = "Alignment"
NOUN = "Noun"
PRO = "Pro"
INDEX = "Verb"


# Parameter iterable: an iterable object
# Output a list of all possible combinations of iterable
def combos(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(2, len(s) + 1))


# Parameter source_set: a set of sources of the format "bibkey:pages", possibly with "page1-page2"
# Output: A single string that compresses all inputs in the format "bibkey1[pages];bibkey2[pages]"
#         and has collapsed all page numbers (e.g., <18-20> + <19-25> = <18-25>)
# This is used to convert the hand-written sources into the format demanded by CLDF
def compress_sources(source_set):
    sources = dict()
    for s in source_set:
        for x in s.split(";"):
            sources[x.split(":")[0].strip()] = dict()
            sources[x.split(":")[0].strip()]["num"] = set()
            sources[x.split(":")[0].strip()]["nonnum"] = set()  # e.g., "145ff"
    for s in list(source_set):
        for x in s.split(";"):
            source_split = x.strip().split(":")
            source_name = source_split[0].strip()
            if len(source_split) > 1:
                for page in source_split[1].split(","):
                    page = re.sub("\u2013", "-", page).strip()  # alternative unicode hyphen
                    if "-" in page:
                        first = page.split("-")[0]
                        last = page.split("-")[1]
                        if first.isnumeric() and last.isnumeric():
                            for y in range(int(first), int(last) + 1):
                                sources[source_name]["num"].add(y)
                        else:
                            sources[source_name]["nonnum"].add(page)
                    elif page.isnumeric():
                        sources[source_name]["num"].add(int(page))
                    else:
                        sources[source_name]["nonnum"].add(page)
    retval = []
    for s in sources.keys():
        numeric = []
        first_num = -1
        last_num = -1
        for x in sorted(sources[s]["num"]):
            if x == last_num + 1:
                last_num = x
            else:
                if last_num != -1:
                    if last_num == first_num:
                        numeric.append(str(last_num))
                    else:
                        numeric.append(str(first_num) + "-" + str(last_num))
                first_num = x
                last_num = x
        if last_num == first_num & last_num != -1:
            numeric.append(str(last_num))
        elif last_num != -1:
            numeric.append(str(first_num) + "-" + str(last_num))
        if len(numeric) == 0:
            if len(sources[s]["nonnum"]) > 0:
                retval.append(s + "[" + ",".join(sorted(list(sources[s]["nonnum"]))) + "]")
            else:
                retval.append(s)
        else:
            retval.append(s + "[" + ",".join(sorted(numeric + list(sources[s]["nonnum"]))) + "]")
    return ";".join(sorted(retval))


# Parameter sources_set: a set of sources of the format "bibkey[pages]"
# Output: A single string that combines all inputs into a single string "bibkey1[pages];bibkey2[pages]"
#         This is done partially by converting the format and calling into compress_sources
def combine_sources(sources_set):
    col_set = []
    for s in sources_set:
        col_set.append(re.sub("\\[", ":", re.sub("\\]", "", s)))
    return compress_sources(col_set)


# Parameter coder_set: a set of coders of the format "HT" or "HT;DI"
# Output: A single string that combines all inputs into a single string "HT;DI"
def combine_coders(coder_set):
    ret_set = set()
    for s in coder_set:
        for x in s.split(";"):
            ret_set.add(x.strip())
    return ";".join(sorted(ret_set))


# Parameters param_loc: The location of the parameters.csv
# Output: param_dict, a dictionary of parameters and questions, in the form of:
#         parameter -> question
def read_parameters_csv(param_loc):
    parameters = pd.read_csv(param_loc, keep_default_na=False)
    paramset = [
        CASE_P,
        INDEX_P,
        TAM_P,
        SPLIT_S_P,
        FEAT_P,
        DOM_P,
        DOM_T_P,
        REF_N_FREQ_P,
        REF_P_FREQ_P,
        REF_I_FREQ_P,
        REF_N_NL_FREQ_P,
        REF_P_NL_FREQ_P,
        REF_I_NL_FREQ_P,
        REF_ERG_P,
        REF_ERG_LOC_P,
        REF_TRI_P,
        REF_TRI_LOC_P,
        REF_HOR_P,
        REF_HOR_LOC_P,
        REF_SENS_P,
        REF_SENS_LOC_P,
        COARG_SENS_F_P,
        COARG_SENS_I_P
    ]
    param_dict = dict()
    for k in paramset:
        param_dict[k] = parameters.loc[parameters.Parameter_ID == k, "Question"].values[0]
    return param_dict


# Parameters ref_loc: The location of the references.csv
# Output: ref_dict, a nested dictionary of reference frequencies, in the form of:
#         glottocode -> [ PRO, NOUN, INDEX ] -> [ alignments ]    -> Frequency of alignment
#                                            -> "Most'            -> Most frequent reference-based alignment
#                                            -> [ Coder, Source ] -> coder, source information
def read_reference_aggregation(ref_loc):
    references = pd.read_csv(ref_loc, keep_default_na=False)
    ref_dict = dict()
    glottocodes = set(references.Glottocode)
    all_alignments = set(references.Alignment.values)
    for g in glottocodes:
        ref_dict[g] = dict()
        for l in [PRO, NOUN, INDEX]:
            ref_dict[g][l] = dict()
            alignments = list(
                references[(references.Glottocode == g) & (references.Domain == l)].Alignment.values
            )
            alignments_not_local = list(
                references[(references.Glottocode == g) & (references.Domain == l)].Alignment_not_local.values
            )
            best_alignment = None
            max_alignment = 0
            for a in all_alignments:
                if a in alignments:
                    freq = float(alignments.count(a)) / len(alignments)
                    if freq == max_alignment:
                        best_alignment += ";" + a
                    elif freq > max_alignment:
                        max_alignment = freq
                        best_alignment = a
                    ref_dict[g][l][a] = freq
                else:
                    ref_dict[g][l][a] = 0
            if (best_alignment is not None):
                best_alignment = ";".join(sorted(best_alignment.split(";")))
            ref_dict[g][l]["Most"] = best_alignment
            #notlocal alignment
            best_alignment_not_local = None
            max_alignment_not_local = 0
            for a in all_alignments:
                if a in alignments_not_local:
                    freq = float(alignments_not_local.count(a)) / len(alignments_not_local)
                    if freq == max_alignment_not_local:
                        best_alignment_not_local += ";" + a
                    elif freq > max_alignment_not_local:
                        max_alignment_not_local = freq
                        best_alignment_not_local = a
                    ref_dict[g][l][a+"_not_local"] = freq
                else:
                    ref_dict[g][l][a+"_not_local"] = 0
            if (best_alignment_not_local is not None):
                best_alignment_not_local = ";".join(sorted(best_alignment_not_local.split(";")))
            ref_dict[g][l]["Most_not_local"] = best_alignment_not_local
    return ref_dict


# Parameters ref_dict: the reference dictionary, generated by the read_reference_aggregation function
# Output lang_ref_dict: a dictionary of language-level aggregations about references
def generate_reference_questions(ref_dict):
    lang_ref_dict = dict()
    for glot in ref_dict.keys():
        lang_ref_dict[glot] = dict()
        lang_ref_dict[glot][REF_N_FREQ_P] = ref_dict[glot][NOUN]["Most"] if ref_dict[glot][NOUN]["Most"] != None else "NA"
        lang_ref_dict[glot][REF_P_FREQ_P] = ref_dict[glot][PRO]["Most"] if ref_dict[glot][PRO]["Most"] != None else "NA"
        lang_ref_dict[glot][REF_I_FREQ_P] = ref_dict[glot][INDEX]["Most"] if ref_dict[glot][INDEX]["Most"] != None else "NA"
        lang_ref_dict[glot][REF_N_NL_FREQ_P] = ref_dict[glot][NOUN]["Most_not_local"] if ref_dict[glot][NOUN]["Most_not_local"] != None else "NA"
        lang_ref_dict[glot][REF_P_NL_FREQ_P] = ref_dict[glot][PRO]["Most_not_local"] if ref_dict[glot][PRO]["Most_not_local"] != None else "NA"
        lang_ref_dict[glot][REF_I_NL_FREQ_P] = ref_dict[glot][INDEX]["Most_not_local"] if ref_dict[glot][INDEX]["Most_not_local"] != None else "NA"
        lang_ref_dict[glot][REF_ERG_P] = (
            True
            if True in [ref_dict[glot][x]["ergative"] > 0 for x in ref_dict[glot].keys()]
            else False
        )
        lang_ref_dict[glot][REF_TRI_P] = (
            True
            if True in [ref_dict[glot][x]["tripartite"] > 0 for x in ref_dict[glot].keys()]
            else False
        )
        lang_ref_dict[glot][REF_HOR_P] = (
            True
            if True in [ref_dict[glot][x]["horizontal"] > 0 for x in ref_dict[glot].keys()]
            else False
        )
        lang_ref_dict[glot][REF_SENS_P] = (
            True
            if True in [ref_dict[glot][x]["sensitive"] > 0 for x in ref_dict[glot].keys()]
            else False
        )
        if lang_ref_dict[glot][REF_ERG_P]:
            lang_ref_dict[glot][REF_ERG_LOC_P] = ";".join(sorted(
                [x for x in ref_dict[glot].keys() if ref_dict[glot][x]["ergative"] > 0]
            ))
        else:
            lang_ref_dict[glot][REF_ERG_LOC_P] = "NA"
        if lang_ref_dict[glot][REF_TRI_P]:
            lang_ref_dict[glot][REF_TRI_LOC_P] = ";".join(sorted(
                [x for x in ref_dict[glot].keys() if ref_dict[glot][x]["tripartite"] > 0]
            ))
        else:
            lang_ref_dict[glot][REF_TRI_LOC_P] = "NA"
        if lang_ref_dict[glot][REF_HOR_P]:
            lang_ref_dict[glot][REF_HOR_LOC_P] = ";".join(sorted(
                [x for x in ref_dict[glot].keys() if ref_dict[glot][x]["horizontal"] > 0]
            ))
        else:
            lang_ref_dict[glot][REF_HOR_LOC_P] = "NA"
        if lang_ref_dict[glot][REF_SENS_P]:
            lang_ref_dict[glot][REF_SENS_LOC_P] = ";".join(sorted(
                [x for x in ref_dict[glot].keys() if ref_dict[glot][x]["sensitive"] > 0]
            ))
        else:
            lang_ref_dict[glot][REF_SENS_LOC_P] = "NA"
    return lang_ref_dict


# Parameters contexts: a pandas dataframe of the contexts.csv
#            selectors: a pandas dataframe of the selectors.csvcsv
#            languages: a pandas dataframe of the languages.csv
#            sensitive_lgs_flagging: a set of all langauges with non-portmanteau-based coargument sensitivity in flagging
#            sensitive_lgs_indexing: a set of all langauges with non-portmanteau-based coargument sensitivity in flagging
# Output: A dictionary <derived> of all language-level aggregations, the dictionary is of the format
#         glottocode : attr_value_dict, for which the attr_value_dict is a dictionary of the format
#         attribute : {VALUE : <attribute value>, SOURCE: <source>, CODER : <coder>}
#             for each attribute, e.g. "presence of case", "presence of indexation", "TAM split", "Split S", "Features targeted"
def basic_language_level(contexts, selectors, languages, sensitive_lgs_flagging, sensitive_lgs_indexing):
    derived = dict()
    glottocodes = set(contexts.Glottocode.values)
    for g in glottocodes:
        derived[g] = dict()
        # The data for the paper includes only languages that have been coded for case + indexing, so the presence of both can be assumed
        coder_case = ";".join(sorted(
            set(selectors[
                    (selectors.Glottocode == g)
                    & (selectors.Selector_type == "flagging")
                ].Coder
            ))
        )
        coder_case = ";".join(sorted(set([x.strip() for x in coder_case.split(";")])))
        source_case = combine_sources(
            set(
                selectors[
                    (selectors.Glottocode == g)
                    & (selectors.Selector_type == "flagging")
                ].Source
            )
        )
        derived[g][CASE_P] = dict()
        derived[g][CASE_P][VALUE] = (
            "?"
            if languages[languages.Glottocode == g]["sufficient_data_flagging"].values[0] == False
            else
            "True"
            if len(
                selectors[
                    (selectors.Glottocode == g)
                    & (selectors.Selector_type == "flagging")
                    & (selectors.Marker_type == "overt")
                ]
            ) > 0
            else "False"
        )
        # derived[g][CASE_P][VALUE] = 'True' if len(selectors[(selectors.Glottocode == g) & (selectors.Selector_type == 'flagging') & (selectors.Marker_type != 'zero')]) > 0 else 'False'
        derived[g][CASE_P][SOURCE] = source_case
        derived[g][CASE_P][CODER] = coder_case
        coder_indexation = ";".join(sorted(
            set(
                selectors[
                    (selectors.Glottocode == g)
                    & (selectors.Selector_type != "flagging")
                ].Coder
            )
        ))
        coder_indexation = ";".join(sorted(
            set([x.strip() for x in coder_indexation.split(";")])
        ))
        source_indexation = combine_sources(
            set(
                selectors[
                    (selectors.Glottocode == g)
                    & (selectors.Selector_type == "flagging")
                ].Source
            )
        )
        derived[g][INDEX_P] = dict()
        derived[g][INDEX_P][VALUE] = (
            "True"
            if len(
                selectors[
                    (selectors.Glottocode == g)
                    & (selectors.Selector_type == "indexing marker")
                    & (selectors.Marker_type == "overt")
                ]
            ) > 0 or languages[languages.Glottocode == g]["sufficient_data_indexing"].values[0] == False
            else "False"
        )
        derived[g][INDEX_P][SOURCE] = source_indexation
        derived[g][INDEX_P][CODER] = coder_indexation
        derived[g][FEAT_P] = dict()
        if languages[languages.Glottocode == g]["sufficient_data_indexing"].values[0] == False:
            derived[g][FEAT_P][VALUE] = "?"
        elif INDEX_P in derived[g].keys():
            derived[g][FEAT_P][VALUE] = (
                "NA" if derived[g][INDEX_P][VALUE] == "False"
                else ";".join(sorted(
                    set(
                        selectors[
                            (selectors.Glottocode == g) & (selectors.Features != "NA")
                        ].Features.values
                    )
                    - set([""])
                ))
            )
        else:
            derived[g][FEAT_P][VALUE] = "NA"
        derived[g][FEAT_P][SOURCE] = source_indexation
        derived[g][FEAT_P][CODER] = coder_indexation
        coder_total = ";".join(set(contexts[(contexts.Glottocode == g)].Coder))
        coder_total = ";".join(sorted(set([x.strip() for x in coder_total.split(";")])))
        source_total = combine_sources(set(contexts[(contexts.Glottocode == g)].Source))
        derived[g][TAM_P] = dict()
        derived[g][TAM_P][VALUE] = (
            "?"
            if languages[languages.Glottocode == g]["sufficient_data_indexing"].values[0] == False
            else
            "True"
            if len(set(contexts[(contexts.Glottocode == g)].TAM.values)) > 1
            else "False"
        )
        derived[g][TAM_P][SOURCE] = source_total
        derived[g][TAM_P][CODER] = coder_total
        derived[g][SPLIT_S_P] = dict()
        derived[g][SPLIT_S_P][VALUE] = (
            "?"
            if languages[languages.Glottocode == g]["sufficient_data_indexing"].values[0] == False
            else
            "True"
            if len(
                contexts[
                    (contexts.Glottocode == g)
                    & (contexts.Role == "S")
                    & (contexts.Predicate_class != "default")
                ]
            ) > 0
            else "False"
        )
        derived[g][SPLIT_S_P][SOURCE] = source_total
        derived[g][SPLIT_S_P][CODER] = coder_total
        derived[g][COARG_SENS_F_P] = dict()
        derived[g][COARG_SENS_I_P] = dict()
        if g in sensitive_lgs_flagging:
            derived[g][COARG_SENS_F_P][VALUE] = "yes"
        else:
            derived[g][COARG_SENS_F_P][VALUE] = "no"
        if g in sensitive_lgs_indexing:
            derived[g][COARG_SENS_I_P][VALUE] = "yes"
        else:
            derived[g][COARG_SENS_I_P][VALUE] = "no"
        derived[g][COARG_SENS_F_P][SOURCE] = source_total
        derived[g][COARG_SENS_F_P][CODER] = coder_total
        derived[g][COARG_SENS_I_P][SOURCE] = source_total
        derived[g][COARG_SENS_I_P][CODER] = coder_total
    return derived


# Parameters contexts: a pandas dataframe of the the contexts.csv
#            selectors: a pandas dataframe of the selectors.csv
# Output selectors: A modified version of the input selectors dataframe, with four added columns:
#                   S_references, A_references, P_references, ;-concatenated lists of S/A/P references the marker indexes
#                   Alignment, the trigger-potential alignment of the morpheme, based on the S/A/P_references columns
def selector_aggregation(contexts, selectors):
    selectors["S_references"] = "NONE"
    selectors["A_references"] = "NONE"
    selectors["P_references"] = "NONE"
    selectors["Alignment"] = "TBD"
    for index, row in contexts.iterrows():
        if row.Role == "any" or row.Role == "none":
            continue
        if (selectors[selectors.ID == row.Selector_ID][row.Role + "_references"].item() == "NONE"):
            selectors.loc[selectors.ID == row.Selector_ID, row.Role + "_references"] = (
                row.Reference + "_" + row["Co-argument_reference"]
                if row["Co-argument_reference"] != "any" and row["Co-argument_reference"] != "NA"
                else row.Reference
            )
        else:
            added_value = (
                row.Reference + "_" + row["Co-argument_reference"]
                if row["Co-argument_reference"] != "any" and row["Co-argument_reference"] != "NA"
                else row.Reference
            )
            refs = set(selectors.loc[selectors.ID == row.Selector_ID, row.Role + "_references"].item().split(';'))
            refs.add(added_value)
            selectors.loc[selectors.ID == row.Selector_ID, row.Role + "_references"] = (
                ";".join(sorted(refs))
            )
    for index, row in selectors.iterrows():
        if row.Selector_type == 'indexing trigger':
            row["Alignment"] = "NA"
            continue
        has_S = row["S_references"] != "NONE"
        has_A = row["A_references"] != "NONE"
        has_P = row["P_references"] != "NONE"
        if has_S == has_A == has_P:
            # If there is no indexing at all, then alignment is NA;
            # otherwise (e.g., even for flagging) {S,A,P} are grouped together 
            #           (maybe only against P,T,G, other roles) and hence neutral
            if row["Selector_type"] == "indexing trigger" and has_S == False:
                row["Alignment"] = "NA"
            else:
                row["Alignment"] = "neutral"
        elif has_S == has_A:
            row["Alignment"] = "accusative"
        elif has_S == has_P:
            row["Alignment"] = "ergative"
        elif has_A == has_P:
            row["Alignment"] = "horizontal"
    return selectors


# Parameters contexts: a pandas dataframe of the the contexts.csv
#            selectors: a pandas dataframe of the selectors.csv
# Output: dom_dict, a dictionary of all the DOM splits that occur, with possibly several entries per language
def dom_aggregation(contexts, selectors, references):
    dom_dict = dict()
    glottocodes = set(contexts.Glottocode.values)
    for g in glottocodes:
        # collect all P-marking contexts for the language, their selector IDs, and whether these markers are overt or zero
        g_data = contexts[(contexts.Glottocode == g) & (contexts.Role == "P") & (contexts.Reference.str.contains("Pro")==False)]
        case_P_selector_ID = set()
        case_P_overt = set()
        for s in set(g_data.Selector_ID):
            if (
                s == "NULL_INDEXING"
                or s == "NULL_FLAGGING"
                or selectors[selectors.ID == s].Selector_type.item() != "flagging"
                or not any("Noun" in x or "any" in x for x in set(contexts[contexts.Selector_ID == s].Reference))
            ):
                continue
            case_P_overt.add(selectors[selectors.ID == s].Marker_type.item())
            case_P_selector_ID.add(s)
        # in case all marking is zero or all marking is identical, there is no DOM
        if (
            len(g_data) == 0
            or case_P_overt == set(["zero"])
            or len(case_P_selector_ID) == 1
        ):
            continue
        # filter g_data only for contexts which mark case on P (this gets rid of P indexation rows)
        g_data = g_data[(g_data.Selector_ID.isin(case_P_selector_ID))]
        # check if changing the P selectors changes alignment
        glot_refs = references[(references.Glottocode == g) & ((references.Domain == "Noun") | (references.Domain == "Pro"))]
        p_to_alignment = dict()
        for selectorid in case_P_selector_ID:
            selector = selectors.Selector_label[selectors.ID == selectorid].values[0]
            selector_alignments = set()
            for index, row in glot_refs.iterrows():
                if selector in row.S or selector in row.A or selector in row.P:
                    to_add = row.Alignment
                    if to_add == "overt neutral":
                        to_add = "neutral"
                    elif to_add == "no marking":
                        to_add = "neutral"
                    selector_alignments.add(to_add)
            p_to_alignment[selector] = selector_alignments
        some_different_alignment = False
        for p in p_to_alignment.keys():
            for q in set(p_to_alignment.keys()) - set([p]):
                if p_to_alignment[p] != p_to_alignment[q]:
                    some_different_alignment = True
        # if two P selectors appear in the same cell, there is coargument sensitive DOM
        coargument_dom = False
        for p_id in case_P_selector_ID:
            p_sel = selectors.Selector_label[selectors.ID == p_id].values[0]
            for q_id in case_P_selector_ID - set([p_id]):
                q_sel = selectors.Selector_label[selectors.ID == q_id].values[0]
                for index, row in glot_refs.iterrows():
                    row_selectors = set([re.sub("_overt.*|_zero.*", "", x).strip() for x in row.P.split(";")])
                    if p_sel in row_selectors and q_sel in row_selectors:
                        coargument_dom = True
        if not some_different_alignment and not coargument_dom:
            continue
        # Collect all columns in which different conditions appear which could be relevant for the DOM split
        dommable = [
            x
            for x in [
                "TAM",
                "Reference",
                "Miscellaneous_condition",
                "Predicate_class",
            ]
            if len(set(g_data[x])) > 1
        ]
        # if there is something coargumenty going on, check and see if it is only present under some condition
        if coargument_dom:
            dommable += ["Co-argument_reference"]
        # Cycle through each individual dommable condition and see if it perfectly splits P into different selector IDs
        # If an individual split works, we will not bother with looking at multi-conditioned DOM splits
        single_works = False
        for cond in dommable:
            divides = True
            alignment = dict() # a dictionary which maps P_marking_form : set(alignments)
            variation = dict() # a dictionary which maps P_marking_form : set(condition_values), e.g. {ACC : set(1Pro), ABS : set(2Pro,3Pro,Noun)}
            vs = set(g_data[cond]) - set( ["any"] )  # all the values that condition cond can have, minus the wildcard 'any'
            for v in vs:
                g_cond_data = g_data[(g_data[cond] == v) | (g_data[cond] == "any")]
                if len(set(g_cond_data.Selector_ID)) != 1:  # this condition does not create divisions between P marking
                    divides = False
                else:  # this condition yields only one P marking
                    val = selectors[selectors.ID == list(set(g_cond_data.Selector_ID))[0]].Selector_label.item()
                    if val not in variation.keys():
                        variation[val] = set()
                    variation[val].add(v)
                    if cond == "Co-argument_reference":
                        glot_filter = glot_refs
                    else:
                        glot_filter = filter_for_conditions(glot_refs, [cond], [v])
                    v_alignments = set()
                    for index, row in glot_filter.iterrows():
                        v_alignments.add(row.Alignment)
                    alignment[v] = v_alignments
            # if this is co-argument reference and divides, then we are golden
            if cond == "Co-argument_reference" and divides:
                single_works = True
                if g not in dom_dict.keys():
                    dom_dict[g] = dict()
                dom_dict[g][cond] = dict()
                dom_dict[g][cond][VALUE] = variation
                dom_dict[g][cond][ALIGNMENT] = {k: set(["incalculable from references.csv: see raw data"]) for k in alignment.keys()}
                dom_dict[g][cond][SOURCE] = combine_sources(set(g_data.Source))
                dom_dict[g][cond][CODER] = list(set(g_data.Coder))[0]
            else:
            # check if this division generates a truly different alignment
                some_different_alignment = False
                for p in alignment.keys():
                    for q in set(alignment.keys()) - set([p]):
                        if alignment[p] != alignment[q]:
                            some_different_alignment = True
                if divides and some_different_alignment:  # put the condition and its variations into the dom_dict structure
                    single_works = True
                    if g not in dom_dict.keys():
                        dom_dict[g] = dict()
                    dom_dict[g][cond] = dict()
                    dom_dict[g][cond][VALUE] = variation
                    dom_dict[g][cond][ALIGNMENT] = alignment
                    dom_dict[g][cond][SOURCE] = combine_sources(set(g_data.Source))
                    dom_dict[g][cond][CODER] = list(set(g_data.Coder))[0]
        if not single_works:  # do the same thing but for all combinations of 2 dommable features, seeing which ones work
            for multicond in combos(dommable):
                multicond = sorted(multicond)
                divides = True
                alignment = dict()
                variation = dict()
                cond1 = multicond[0]
                cond2 = multicond[1]
                values = [set(g_data[c]) - set(["any"]) for c in multicond]
                value_combo = list(product(*values))
                for vals in value_combo:
                    g_cond_data = g_data
                    for i in range(0, len(vals)):
                        g_cond_data = g_cond_data[(g_cond_data[multicond[i]] == vals[i]) | (g_cond_data[multicond[i]] == "any")]
                    if len(set(g_cond_data.Selector_ID)) > 1:
                        divides = False
                    else:
                        var_val = selectors[selectors.ID == list(set(g_cond_data.Selector_ID))[0]].Selector_label.item()
                        if var_val not in variation.keys():
                            variation[var_val] = set()
                        variation[var_val].add("+".join(vals))
                        glot_filter = glot_refs
                        for i in range(0, len(vals)):
                            if multicond[i] != "Co-argument_reference":
                                glot_filter = filter_for_conditions(glot_filter, [multicond[i]], [vals[i]])
                        v_alignments = set()
                        for index, row in glot_filter.iterrows():
                            v_alignments.add(row.Alignment)
                        alignment["+".join(vals)] = v_alignments
                # check if this division generates a different alignment
                if ("Co-argument_reference" in multicond) and divides:
                    cond_key = "+".join(multicond)
                    if g not in dom_dict.keys():
                        dom_dict[g] = dict()
                    dom_dict[g][cond_key] = dict()
                    dom_dict[g][cond_key][VALUE] = variation
                    dom_dict[g][cond_key][ALIGNMENT] = {k: set(["incalculable from references.csv: see raw data"]) for k in alignment.keys()}
                    dom_dict[g][cond_key][SOURCE] = combine_sources(set(g_data.Source))
                    dom_dict[g][cond_key][CODER] = list(set(g_data.Coder))[0]
                else:
                    some_different_alignment = False
                    for p in alignment.keys():
                        for q in set(alignment.keys()) - set([p]):
                            if alignment[p] != alignment[q]:
                                some_different_alignment = True
                    if divides and some_different_alignment:
                        cond_key = "+".join(multicond)
                        if g not in dom_dict.keys():
                            dom_dict[g] = dict()
                        dom_dict[g][cond_key] = dict()
                        dom_dict[g][cond_key][VALUE] = variation
                        dom_dict[g][cond_key][ALIGNMENT] = alignment
                        dom_dict[g][cond_key][SOURCE] = combine_sources(set(g_data.Source))
                        dom_dict[g][cond_key][CODER] = list(set(g_data.Coder))[0]
    return dom_dict


# Parameters references_df: the references dataframe
#            conditions: an array of conditions on which the dataframe should be filtered
#            values: an array of the individual values that each condition has
# Output: Returns the references_df filtered by conditions and values
def filter_for_conditions(references_df, conditions, values):
    assert(len(conditions) == len(values))
    ret_filter = references_df
    for i in range(0, len(conditions)):
        if conditions[i] == "TAM":
            ret_filter = ret_filter[ret_filter.TAM == values[i]]
        elif conditions[i] == "Reference":
            ret_filter = ret_filter[ret_filter.Referential_type == values[i]]
        elif conditions[i] == "Co-argument_reference":
            ret_filter = ret_filter
        elif conditions[i] == "Miscellaneous_condition":
            ret_filter = ret_filter[ret_filter.Condition == values[i]]
        elif conditions[i] == "Predicate_class":
            ret_filter = ret_filter[(ret_filter.Monovalent_predicate_class == values[i]) | (ret_filter.Bivalent_predicate_class == values[i])]
    return ret_filter


# Parameters dom_dict: the DOM dictionary outputted by the dom_aggregation function
#            doms_loc: the location that doms.csv should be written to
# Output: Writes to "generic-cldf/doms.csv" the DOM dictionary as a CLDF-compliant table,
#         complete with an ID (glottocode_unique-number) and marking and condition separated into separate columns
def write_doms_csv(dom_dict, doms_loc):
    max_split = 0
    for g in dom_dict.keys():
        for s in dom_dict[g].keys():
            if max_split < len(dom_dict[g][s][VALUE].keys()):
                max_split = len(dom_dict[g][s][VALUE].keys())
    marking_cols = []
    for x in range(1, max_split + 1):
        marking_cols.append("Marking_" + str(x))
        marking_cols.append("Alignments_" + str(x))
        marking_cols.append("Condition_" + str(x))
    with open(doms_loc, "w+") as f:
        writer = csv.writer(f)
        writer.writerow(["ID", "Glottocode", "Conditioning"] + marking_cols + ["Source", "Coder"])
        for g in sorted(dom_dict):
            counter = 1
            for cond in sorted(dom_dict[g]):
                m_cols = []
                for k in sorted(dom_dict[g][cond][VALUE]):
                    m_cols.append(k)
                    alignments = set()
                    for v in dom_dict[g][cond][VALUE][k]:
                        alignments.update(dom_dict[g][cond][ALIGNMENT][v])
                    m_cols.append(";".join(sorted(alignments)))
                    m_cols.append(";".join(sorted(dom_dict[g][cond][VALUE][k])))
                m_cols += [""] * (max_split * 3 - len(m_cols))
                writer.writerow(
                    [g + "-" + str(counter), g, cond]
                    + m_cols
                    + [dom_dict[g][cond][SOURCE], dom_dict[g][cond][CODER]]
                )
                counter += 1


# Parameters lang_dict: the set of all language-level aggregations, generated by the language_level function
#            values_loc: the location where values.csv should be written to
#            dom_dict (opt): the set of all role-level aggregations of P (DOM), generated by the dom_aggregation function
#            ref_dict (opt): the set of all reference-level aggregations (references.csv), generated by the read_reference_aggregation function
# Output: Writes to file structure-cldf/values.csv, which writes all the language-level aggregations present in
#         the lang_dict, as well as the presence of DOM in the language, derived from dom_dict
def write_values_csv(lang_dict, values_loc, dom_dict=None, ref_dict=None):
    with open(values_loc, "w+") as f:
        writer = csv.writer(f)
        writer.writerow(["ID", "Glottocode", "Parameter_ID", "Value", "Coder", "Source"])
        for glot in sorted(lang_dict.keys()):
            for param in lang_dict[glot].keys():
                writer.writerow(
                    [
                        glot + "-" + param,
                        glot,
                        param,
                        lang_dict[glot][param][VALUE],
                        lang_dict[glot][param][CODER],
                        lang_dict[glot][param][SOURCE],
                    ]
                )
        if dom_dict is not None:
            for glot in sorted(lang_dict.keys()):
                if glot not in dom_dict.keys() and CASE_P in lang_dict[glot].keys():
                    writer.writerow(
                        [
                            glot + "-" + DOM_P,
                            glot,
                            DOM_P,
                            "No",
                            lang_dict[glot][CASE_P][CODER],
                            lang_dict[glot][CASE_P][SOURCE],
                        ]
                    )
                    writer.writerow(
                        [
                            glot + "-" + DOM_T_P,
                            glot,
                            DOM_T_P,
                            "NA",
                            lang_dict[glot][CASE_P][CODER],
                            lang_dict[glot][CASE_P][SOURCE],
                        ]
                    )
            for glot in sorted(dom_dict.keys()):
                dom_coder = [dom_dict[glot][k][CODER] for k in dom_dict[glot].keys()][0]
                dom_source = [dom_dict[glot][k][SOURCE] for k in dom_dict[glot].keys()][0]
                writer.writerow(
                    [
                        glot + "-" + DOM_P, 
                        glot, 
                        DOM_P, 
                        "Yes", 
                        dom_coder, 
                        dom_source
                    ]
                )
                writer.writerow(
                    [
                        glot + "-" + DOM_T_P,
                        glot,
                        DOM_T_P,
                        ";".join(sorted(dom_dict[glot].keys())),
                        dom_coder,
                        dom_source,
                    ]
                )
        if ref_dict is not None:
            lang_ref_dict = generate_reference_questions(ref_dict)
            for glot in sorted(lang_ref_dict.keys()):
                flagging_coder = lang_dict[glot][CASE_P][CODER]
                flagging_source = lang_dict[glot][CASE_P][SOURCE]
                indexing_coder = lang_dict[glot][INDEX_P][CODER]
                indexing_source = lang_dict[glot][INDEX_P][SOURCE]
                all_coders = combine_coders(set([indexing_coder] + [flagging_coder])-set([""]))
                all_sources = combine_sources(set([flagging_source] + [indexing_source])-set([""]))
                writer.writerow(
                    [
                        glot + "-" + REF_N_FREQ_P,
                        glot,
                        REF_N_FREQ_P,
                        lang_ref_dict[glot][REF_N_FREQ_P],
                        flagging_coder,
                        flagging_source,
                    ]
                )
                writer.writerow(
                    [
                        glot + "-" + REF_P_FREQ_P,
                        glot,
                        REF_P_FREQ_P,
                        lang_ref_dict[glot][REF_P_FREQ_P],
                        flagging_coder,
                        flagging_source,
                    ]
                )
                writer.writerow(
                    [
                        glot + "-" + REF_I_FREQ_P,
                        glot,
                        REF_I_FREQ_P,
                        lang_ref_dict[glot][REF_I_FREQ_P],
                        indexing_coder,
                        indexing_source,
                    ]
                )
                writer.writerow(
                    [
                        glot + "-" + REF_N_NL_FREQ_P,
                        glot,
                        REF_N_NL_FREQ_P,
                        lang_ref_dict[glot][REF_N_NL_FREQ_P],
                        flagging_coder,
                        flagging_source,
                    ]
                )
                writer.writerow(
                    [
                        glot + "-" + REF_P_NL_FREQ_P,
                        glot,
                        REF_P_NL_FREQ_P,
                        lang_ref_dict[glot][REF_P_NL_FREQ_P],
                        flagging_coder,
                        flagging_source,
                    ]
                )
                writer.writerow(
                    [
                        glot + "-" + REF_I_NL_FREQ_P,
                        glot,
                        REF_I_NL_FREQ_P,
                        lang_ref_dict[glot][REF_I_NL_FREQ_P],
                        indexing_coder,
                        indexing_source,
                    ]
                )
                for param in [
                    REF_ERG_P,
                    REF_ERG_LOC_P,
                    REF_TRI_P,
                    REF_TRI_LOC_P,
                    REF_HOR_P,
                    REF_HOR_LOC_P,
                    REF_SENS_P,
                    REF_SENS_LOC_P,
                ]:
                    writer.writerow(
                        [
                            glot + "-" + param,
                            glot,
                            param,
                            lang_ref_dict[glot][param],
                            all_coders,
                            all_sources,
                        ]
                    )


# Parameters lang_dict: the set of all language-level aggregations, generated by the language_level function
#            out_loc: the location where human_readable.csv should be written to
#            param_loc: the location of the parameters.csv, which will be read
#            dom_dict (opt): the set of all role-level aggregations of P (DOM), generated by the dom_aggregation function
#            ref_dict (opt): the set of all reference-level aggregations (references.csv), generated by the read_reference_aggregation function
# Output: Writes a human-readable version of values.csv to out_loc, which is structured as one row per
#         language, with each column representing a different question
def write_human_readable_csv(lang_dict, out_loc, param_dict, dom_dict=None, ref_dict=None):
    with open(out_loc, "w+") as f:
        writer = csv.writer(f)
        writer.writerow(["Glottocode"] + list(param_dict.keys()))
        writer.writerow([""] + [param_dict[x] for x in param_dict.keys()])
        for glot in sorted(lang_dict.keys()):
            out_row = [glot]
            for param in [CASE_P, INDEX_P, TAM_P, SPLIT_S_P, FEAT_P]:
                if param in lang_dict[glot].keys():
                    out_row.append(lang_dict[glot][param][VALUE])
                else:
                    out_row.append("")
            if dom_dict is not None:
                if glot in dom_dict.keys():
                    out_row.append("True")
                    out_row.append(";".join(sorted(dom_dict[glot].keys())))
                else:
                    out_row.append("False")
                    out_row.append("NA")
            if ref_dict is not None:
                lang_ref_dict = generate_reference_questions(ref_dict)
                for param in list(param_dict.keys())[7:-2]:
                    if glot in lang_ref_dict.keys():
                        out_row.append(lang_ref_dict[glot][param])
                    else:
                        out_row.append('UNCODED')
            if COARG_SENS_F_P in lang_dict[glot].keys():
                out_row.append(lang_dict[glot][COARG_SENS_F_P][VALUE])
            if COARG_SENS_I_P in lang_dict[glot].keys():
                out_row.append(lang_dict[glot][COARG_SENS_I_P][VALUE])
            writer.writerow(out_row)


# Parameters values_loc: the location of the values.csv file, which will be read
#            selectors_loc: the location of the selectors.csv file, which will be read
#            param_loc: the location of the parameters.csv file, which will be read
#            ref_loc: the location of the references.csv file, which will be read
#            summary_loc: the location at which to write the summary statistics
# Output: Writes a summary statistic csv to summary_loc, which is a table with:
#         Headers: Level, Parameter, Value, Count, Percentage
#         and rows which present three types of summaries:
#         1. Language-level summaries, summarizing everything reported in values.csv
#         2. Selector-level summaries, summarizing over selectors (presence of marking)
#         3. Reference-level summaries, summarizing only the presence of zero indexing per person {1,2,3} per role {S,A,P}
def write_summary_statistics(values_loc, selectors_loc, param_loc, ref_loc, summary_loc):
    values_csv = pd.read_csv(values_loc, keep_default_na=False)
    selectors_csv = pd.read_csv(selectors_loc, keep_default_na=False)
    param_dict = read_parameters_csv(param_loc)
    references_csv = pd.read_csv(ref_loc, keep_default_na=False)
    # Summarize language level statistics by reading in values.csv, counting over each parameter
    lang_stats = dict()
    for k in param_dict.keys():
        lang_stats[k] = dict()
        for value in set(values_csv[(values_csv.Parameter_ID == k) & (values_csv.Value != "NA")].Value.values):
            lang_stats[k][value] = len(values_csv[(values_csv.Parameter_ID == k) & (values_csv.Value == value)])
    # Summarize selector level statistics by reading in selectors.csv, counting the presence of
    # overt marking for each role {S,A,P} for both flagging and indexing.
    selector_stats = dict()
    for locus in ["flagging", "indexing marker"]:
        selector_stats[locus] = dict()
        overt_count = len(selectors_csv[(selectors_csv.Selector_type == locus) & (selectors_csv.Marker_type == "overt")])
        for role in ["S", "A", "P"]:
            selector_stats[locus][role] = {"True": dict(), "False": dict()}
            selector_stats[locus][role]["True"]["count"] = len(
                selectors_csv[
                    (selectors_csv.Selector_type == locus)
                    & (selectors_csv.Marker_type == "overt")
                    & (selectors_csv[role + "_references"] != "NONE")
                ]
            )
            selector_stats[locus][role]["True"]["percent"] = (
                float(selector_stats[locus][role]["True"]["count"]) / overt_count
            )
            selector_stats[locus][role]["False"]["count"] = len(
                selectors_csv[
                    (selectors_csv.Selector_type == locus)
                    & (selectors_csv.Marker_type == "overt")
                    & (selectors_csv[role + "_references"] == "NONE")
                ]
            )
            selector_stats[locus][role]["False"]["percent"] = (
                float(selector_stats[locus][role]["False"]["count"]) / overt_count
            )
        for alignment in set(
            selectors_csv[
                (selectors_csv.Selector_type == locus)
                & (selectors_csv.Marker_type == "overt")
            ].Alignment.values
        ):
            selector_stats[locus][alignment] = dict()
            selector_stats[locus][alignment]["count"] = len(
                selectors_csv[
                    (selectors_csv.Selector_type == locus)
                    & (selectors_csv.Marker_type == "overt")
                    & (selectors_csv.Alignment == alignment)
                ]
            )
            selector_stats[locus][alignment]["percent"] = (
                float(selector_stats[locus][alignment]["count"]) / overt_count
            )
    # Summarize zero markers at the level of references, only breaking things down by person {1,2,3}
    zero_stats = dict()
    for pers in ["1", "2", "3"]:
        zero_stats[pers] = dict()
        for role in ["S", "A", "P"]:
            zero_stats[pers][role] = dict()
            ref_count = len(
                references_csv[
                    (references_csv.Domain == INDEX)
                    & (references_csv.Exemplar == "all")
                    & (references_csv[role] != "ROLE_NOT_MARKED_zero")
                    & (
                        (references_csv.Referential_type.str.startswith(pers))
                        | (references_csv.Referential_type == "any")
                    )
                    & (references_csv.Alignment != "no marking")
                ]
            )
            zero_count = len(
                references_csv[
                    (references_csv.Domain == INDEX)
                    & (references_csv.Exemplar == "all")
                    & (references_csv[role].str.contains("zero"))
                    & (references_csv[role] != "ROLE_NOT_MARKED_zero")
                    & (
                        (references_csv.Referential_type.str.startswith(pers))
                        | (references_csv.Referential_type == "any")
                    )
                    & (references_csv.Alignment != "no marking")
                ]
            )
            zero_stats[pers][role]["count"] = zero_count
            zero_stats[pers][role]["percent"] = float(zero_count) / ref_count
    with open(summary_loc, "w+") as f:
        writer = csv.writer(f)
        writer.writerow(["Level", "Variable", "Value", "Count", "Percentage"])
        for k in sorted(lang_stats.keys()):
            for v in sorted(lang_stats[k].keys()):
                writer.writerow(
                    [
                        "Language",
                        k,
                        v,
                        lang_stats[k][v],
                        float(lang_stats[k][v])
                        / sum(
                            [
                                lang_stats[k][v]
                                for v in lang_stats[k].keys()
                                if v != "NA"
                            ]
                        ),
                    ]
                )
        for locus in sorted(selector_stats.keys()):
            for role in ["S", "A", "P"]:
                for presence in sorted(selector_stats[locus][role].keys()):
                    writer.writerow(
                        [
                            "Selector (" + locus + ")",
                            role,
                            presence,
                            selector_stats[locus][role][presence]["count"],
                            selector_stats[locus][role][presence]["percent"],
                        ]
                    )
            for alignment in sorted(set(selector_stats[locus].keys()) - set(["S", "A", "P"])):
                writer.writerow(
                    [
                        "Selector (" + locus + ")",
                        "Alignment",
                        alignment,
                        selector_stats[locus][alignment]["count"],
                        selector_stats[locus][alignment]["percent"],
                    ]
                )
        for pers in sorted(zero_stats.keys()):
            for role in sorted(zero_stats[pers].keys()):
                writer.writerow(
                    [
                        "Reference",
                        "Zero indexing of " + pers,
                        "Role " + role,
                        zero_stats[pers][role]["count"],
                        zero_stats[pers][role]["percent"],
                    ]
                )


# Parameters values_list: a list of alignment values from the references.csv which includes an else case
#            possible_coargs: a list of valid possible coarguments for this set of values
# Output: a list of values, with the else case exploded into all possible coarguments
def expand_else(values_list, possible_coargs):
    non_else_persons = [re.sub(".*:", "", x) for x in values_list if ":else" not in x]
    the_else = [x for x in values_list if ":else" in x][0]
    else_marking = re.sub(":else", "", the_else)
    else_persons = set(possible_coargs) - set(non_else_persons)
    values_set = set(values_list) - set([the_else])
    for person in else_persons:
        values_set.add(else_marking + ":" + person)
    return list(values_set)


# Parameters values_list: a list of alignment values from the references.csv, with <else> removed
# Output: the same values_list with all non-3rd coarguments removed
def fix_3(values_list):
    ret_val = [x for x in values_list if "coarg:" not in x or "3" in re.sub(".*_coarg:","",x)]
    # if all are zero, return zero
    if all(["_zero_" in x for x in ret_val]):
        return ["INFERRED_NULL_zero"]
    # if all are the same, return a singular element
    if len(set([re.sub("_coarg:.*","",x) for x in ret_val])) == 1:
        return [re.sub("(_slot:.*)?(_coarg:.*)?","",ret_val[0])]
    return ret_val


# Parameters values_list: a list of alignment values from the references.csv, with <else> removed
# Output: the same values_list with all local scenarios removed
def fix_not_local(values_list, reftype):
    if "3" in reftype:
        return values_list
    else: # non-3rd, remove 1, 2, and incl
        ret_val = [x for x in values_list if "coarg:" not in x or ("1" not in re.sub(".*_coarg:","",x) and "2" not in re.sub(".*_coarg:","",x) and "incl" not in re.sub(".*_coarg:","",x))]
        if all(["_zero_" in x for x in ret_val]):
            return ["INFERRED_NULL_zero"]
        if len(set([re.sub("_coarg:.*","",x) for x in ret_val])) == 1:
            return [re.sub("(_slot:.*)?(_coarg:.*)?","",ret_val[0])]
        return ret_val


# Parameters ref_loc: the location of the references.csv
# Output: Writes to ref_loc additional alignments
def other_alignments(ref_loc):
    references = pd.read_csv(ref_loc, keep_default_na=False)
    if "Alignment_not_local" not in references:
        references.insert(references.columns.get_loc("Alignment") + 1, "Alignment_not_local", "")
    for index, row in references.iterrows():
        glot    = row.Glottocode
        domain  = row.Domain
        reftype = row.Referential_type
        possible_coargs = set(references[(references.Glottocode == glot) & (references.Domain == domain)].Referential_type.values)
        if "1" in reftype or "2" in reftype or "incl" in reftype:
            possible_coargs = [x for x in possible_coargs if "incl" not in x]
        if "1" in reftype or "incl" in reftype:
            possible_coargs = [x for x in possible_coargs if "1" not in x]
        if "2" in reftype or "incl" in reftype:
            possible_coargs = [x for x in possible_coargs if "2" not in x]
        if "high" in reftype:
            in_both_places = "high" in row.A or "high" in row.P
            if not in_both_places:
                possible_coargs = [x for x in possible_coargs if "high" not in x]
        if "low" in reftype:
            in_both_places = "low" in row.A or "high" in row.P
            if not in_both_places:
                possible_coargs = [x for x in possible_coargs if "low" not in x]
        S_values = row.S.split(";")
        S_values = [x.strip() for x in S_values]
        S_values = [re.sub("_slot:[^_\&]+","",x) for x in S_values]
        if (any([":else" in x for x in S_values])):
            S_values = expand_else(S_values, possible_coargs)
        A_values = row.A.split(";")
        A_values = [x.strip() for x in A_values]
        A_values = [re.sub("_slot:[^_\&]+","",x) for x in A_values]
        if (any([":else" in x for x in A_values])):
            A_values = expand_else(A_values, possible_coargs)
        P_values = row.P.split(";")
        P_values = [x.strip() for x in P_values]
        P_values = [re.sub("_slot:[^_\&]+","",x) for x in P_values]
        if (any([":else" in x for x in P_values])):
            P_values = expand_else(P_values, possible_coargs)
        # get rid of zeros that are coordinate with something overt
        S_values = [re.sub("[^\&]*_zero[^&]*\&\&( )|( )\&\&[^&]*zero[^&]*","",x) for x in S_values]
        A_values = [re.sub("[^\&]*_zero[^&]*\&\&( )|( )\&\&[^&]*zero[^&]*","",x) for x in A_values]
        P_values = [re.sub("[^\&]*_zero[^&]*\&\&( )|( )\&\&[^&]*zero[^&]*","",x) for x in P_values]
        # generate alignment with local scenarios removed
        A_values_not_local = fix_not_local(A_values, reftype)
        P_values_not_local = fix_not_local(P_values, reftype)
        S_values = [x if "_zero" not in x else "_zero" for x in S_values]
        A_values_not_local = [x if "_zero" not in x else "_zero" for x in A_values_not_local]
        P_values_not_local = [x if "_zero" not in x else "_zero" for x in P_values_not_local]
        # get rid of duplication
        S_values = list(set(S_values))
        A_values_not_local = list(set(A_values_not_local))
        P_values_not_local = list(set(P_values_not_local))
        if all("_zero" in x and "_overt" not in x for x in S_values) and all("_zero" in x and "_overt" not in x for x in A_values_not_local) and all("_zero" in x and "_overt" not in x for x in P_values_not_local):
            alignment_not_local = "no marking"
        elif any("coarg:" in x for x in S_values) or any("coarg:" in x for x in A_values_not_local) or any("coarg:" in x for x in P_values_not_local):
            alignment_not_local = "sensitive"
        elif S_values == A_values_not_local != P_values_not_local:
            alignment_not_local = "accusative"
        elif S_values == A_values_not_local == P_values_not_local:
            if "_zero" in S_values[0] and not "_overt" in S_values[0]:
                alignment_not_local = "no marking"
            elif "_overt" in S_values[0]:
                alignment_not_local = "overt neutral"
        elif P_values_not_local == A_values_not_local != S_values:
            alignment_not_local = "horizontal"
        elif P_values_not_local == S_values != A_values_not_local:
            alignment_not_local = "ergative"
        elif P_values_not_local != A_values_not_local != S_values:
            alignment_not_local = "tripartite"
        row.Alignment_not_local = alignment_not_local
    references["ID"] = references["ID"].apply(slugify)
    references.to_csv(ref_loc, index=False)
    return references


# Parameters references: a pandas dataframe of references.csv
#            contexts: a pandas dataframe of contexts.csv
#            selectors: a pandas dataframe of selectors.csv
# Output: a set of languages with coargument sensitivity not based exclusively on portmanteaus
def coarg_sensitivity(references, contexts, selectors):
    sensitive_lgs = set()
    for index, row in references.iterrows():
        A_values = [x.strip() for x in row.A.split(";")]
        P_values = [x.strip() for x in row.P.split(";")]
        if any(["ERROR" in x for x in A_values]) or any(["ERROR" in x for x in P_values]): #skip
            continue
        glot = row.Glottocode
        for transitive_values in [A_values, P_values]:
            # explode & states
            for marker in transitive_values.copy():
                if "&&" in marker:
                    transitive_values.remove(marker)
                    coarg_component = re.sub(".*_coarg:","",marker)
                    morphs = [re.sub("_coarg:.*","",x).strip() for x in marker.split("&&")]
                    if "coarg" in marker:
                        coarg_component = re.sub(".*_coarg:","",marker)
                        morphs = [m + "_coarg:" + coarg_component for m in morphs]
                    transitive_values = transitive_values + morphs
            # remove portmanteaus
            for marker in transitive_values.copy():
                if "INFERRED_NULL" in marker or marker == "ROLE_NOT_MARKED_zero" or marker == "NO_PRONOUN":
                    continue
                if ("NULL_MARKER" in marker):
                    selector_ID = selectors[(selectors.Selector_label == "NULL_MARKER") & (selectors.Glottocode == glot)].ID.values[0]
                elif("NO_FLAGGING" in marker):
                    selector_ID = selectors[(selectors.Selector_label == "NO_FLAGGING") & (selectors.Glottocode == glot)].ID.values[0]
                else:
                    selector_ID = selectors[(selectors.Selector_label == re.sub("_zero|_overt|_coarg:.*|_slot:.*","",marker)) & (selectors.Glottocode == glot)].ID.values[0]
                argument    = row.Referential_type
                if ("coarg:" not in marker):
                    transitive_values.remove(marker)
                else:
                    coarg       = re.sub(".*coarg:","",marker)
                    portmanteau = contexts[
                        (contexts.Selector_ID == selector_ID) & 
                        ((contexts.Reference == argument) | 
                        (contexts.Reference == "any")) & 
                        ((contexts["Co-argument_reference"] == coarg) | 
                        (contexts["Co-argument_reference"] == "any"))
                    ].Portmanteau.values[0]
                    if portmanteau == "portmanteau":
                        transitive_values.remove(marker)
            # if more than one remains and they are different it is sensitive
            if len(transitive_values) > 1:
                selector_labels = set([re.sub("(_slot.*)?(_coarg.*)?","",x) for x in transitive_values])
                if len(selector_labels) > 1 | all(["_zero" in x for x in selector_labels]):
                    sensitive_lgs.add(glot)
    return sensitive_lgs


# Parameters contexts: a pandas dataframe of contexts.csv
#            selectors: a pandas dataframe of selectors.csv
# Output: a modified set of contexts and selectors which are filtered for flagging only
def filter_for_flagging(contexts, selectors):
    selectors_flagging = selectors[(selectors.Selector_type == "flagging")]
    contexts_flagging = contexts[(contexts.Selector_ID.isin(set(selectors_flagging.ID)))]
    return contexts_flagging, selectors_flagging


# Parameters contexts: a pandas dataframe of contexts.csv
#            selectors: a pandas dataframe of selectors.csv
# Output: a modified set of contexts and selectors which are filtered for indexing only
def filter_for_indexing(contexts, selectors):
    selectors_indexing = selectors[(selectors.Selector_type.isin(set(["indexing marker", "indexing trigger"])))]
    contexts_indexing = contexts[(contexts.Selector_ID.isin(set(selectors_indexing.ID)))]
    return contexts_indexing, selectors_indexing


if __name__ == "__main__":
    # get all locations
    contexts_loc   = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "generic-cldf" / "contexts.csv")
    selectors_loc  = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "generic-cldf" / "selectors.csv")
    references_loc = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "generic-cldf" / "references.csv")
    doms_loc       = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "generic-cldf" / "doms.csv")
    languages_loc  = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "generic-cldf" / "languages.csv") #both are identical
    metadata_loc   = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "generic-cldf" / "metadata.json")
    parameters_loc = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "structure-cldf" / "parameters.csv")
    values_loc     = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "structure-cldf" / "values.csv")
    human_read_loc = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "human-readable.csv")
    summary_loc    = str(Path(__file__).parent / ".." / ".." / "cldf-alignment" / "summary.csv")
    # read in primary data
    contexts = pd.read_csv(contexts_loc, keep_default_na=False)
    selectors = pd.read_csv(selectors_loc, keep_default_na=False)
    languages = pd.read_csv(languages_loc, keep_default_na=False)
    # generate selector alignment
    selectors = selector_aggregation(contexts, selectors)
    selectors.to_csv(selectors_loc, index=False)
    # generate references.csv
    if Path(references_loc).exists():
        Path(references_loc).unlink()
    write_intermediate_csv(
        outfile=references_loc,
        types=["Verb", "Noun", "Pro"],
        metadata=metadata_loc,
    )
    # generate additional alignments, write references.csv back out
    new_refs = other_alignments(references_loc)
    new_refs_header = new_refs.columns.values.tolist()
    new_refs_values = new_refs.values.tolist()
    new_refs_values.insert(0,new_refs_header)
    new_refs.to_csv(references_loc, index=False)
    # sort references.csv so it is reproduceable run-to-run
    refs = pd.read_csv(references_loc)
    refs = refs.sort_values(by = "ID")
    refs.to_csv(references_loc, index=False)
    # generate doms.csv
    dom_dict = dom_aggregation(contexts, selectors, refs)
    write_doms_csv(dom_dict, doms_loc)
    # look for coargument sensitivity that isn't portmanteaus
    contexts_flagging, selectors_flagging = filter_for_flagging(contexts, selectors)
    contexts_indexing, selectors_indexing = filter_for_indexing(contexts, selectors)
    references = pd.read_csv(references_loc, keep_default_na=False)
    sensitive_lgs_flagging = coarg_sensitivity(references[(references.Domain.isin(set(["Noun","Pronoun"])))], contexts_flagging, selectors_flagging)
    sensitive_lgs_indexing = coarg_sensitivity(references[(references.Domain == "Verb")], contexts_indexing, selectors_indexing)
    # read information from existing csvs in preparation for language level aggregation
    ref_dict = read_reference_aggregation(references_loc)
    param_dict = read_parameters_csv(parameters_loc)
    # generate language level aggregations
    lang_dict = basic_language_level(contexts, selectors, languages, sensitive_lgs_flagging, sensitive_lgs_indexing)
    write_values_csv(lang_dict, values_loc, dom_dict=dom_dict, ref_dict=ref_dict)
    write_human_readable_csv(lang_dict, human_read_loc, param_dict, dom_dict=dom_dict, ref_dict=ref_dict)
    # generate summary statistics, this can be done independently of running the above
    write_summary_statistics(
        values_loc,
        selectors_loc,
        parameters_loc,
        references_loc,
        summary_loc,
    )
