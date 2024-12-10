import typing as t
from csv import DictWriter
from pathlib import Path
from collections import defaultdict
from os import remove
import openpyxl as op
import re
import pycldf

from util import SimpleContainer

E_GLOT = "Glottocode"
E_LAN = "Language"
E_BIR = "Exemplar"
E_REF = "Reference"
E_ROLE = "Role"
E_CO = "Co-argument_reference"
E_SEL = "Selector_label"
E_SEL_TYPE = "Selector_type"
E_OV = "Marker_type"
E_MISC = r"Miscellaneous_condition"
E_TAM = "TAM"
E_PRED = r"Predicate_class"
E_COD = "Coder"
E_SLOT = "Slot"
E_FEAT = "Features"
E_SOURCE = "Source"
E_SEL_ID = "Selector_ID"

I_ID = "ID"
I_GLOT = "Glottocode"
I_LAN = "Language"
I_LOCUS = "Domain"
I_TYP = "Referential_type"
I_PRIM = "Exemplar"
I_S = "S"
I_A = "A"
I_P = "P"
I_ALIG = "Alignment"
I_TAM = "TAM"
I_MONO = r"Monovalent_predicate_class"
I_BI = r"Bivalent_predicate_class"
I_MISC = "Condition"
I_SOURCE = "Source"
I_COD = "Coder"

INTERMEDIATE_HEADER = [
    I_ID,
    I_GLOT,
    I_LAN,
    I_LOCUS,
    I_TYP,
    I_PRIM,
    I_MONO,
    I_BI,
    I_TAM,
    I_MISC,
    I_S,
    I_A,
    I_P,
    I_ALIG,
    I_SOURCE,
    I_COD,
]

COUNTER = 1


TYPE_TO_SELECTOR_TYPE = {
    "Noun": ["flagging"],
    "Pro": ["flagging"],
    "Verb": ["indexing marker","indexing trigger"],
}


# Selector_type is case, then it is used in nouns and pronouns
# Selector_type is agreement marker -> verbs
def yield_from_cldf(
    dataset: pycldf.Dataset,
    type: str,
    errors: SimpleContainer,
):
    current_language = ""
    language_rows = []
    selector_for_this_type = TYPE_TO_SELECTOR_TYPE[type]
    # load languages
    glotto_code_to_language = {}
    for language in dataset["languages.csv"]:
        glotto_code_to_language[language[E_GLOT]] = language["Name"]
    # load selectors.csv; selector_id : row
    selector_id_to_selector_row = {}
    for selector_row in dataset["selectors.csv"]:
        selector_id_to_selector_row[selector_row["ID"]] = selector_row
    # set first language glottocode
    for row in dataset["contexts.csv"]:
        current_language = row[E_GLOT]
        break
    # yielding data, adding fields from languages and selectors
    for row in dataset["contexts.csv"]:
        try:
            row[E_LAN] = glotto_code_to_language[row[E_GLOT]]
        except KeyError:
            errors.log(
                row=(
                    type,
                    "???",
                    row[E_GLOT],
                    ", ".join(sorted(row[E_COD])),
                    f"Glottocode of this row (ID: {row['ID']}) is not in languages.csv. This row was skipped. Fix the original csv to resolve.",
                )
            )
            continue
        # replace domain of type list by string
        row[E_MISC] = ";".join(sorted(row[E_MISC]))
        # add data from selectors.csv
        selector_id = row[E_SEL_ID]
        # some selector ids are None, warn and skip
        # try:
        selector_row = selector_id_to_selector_row[selector_id]
        # we are not calculating non-person based alignments: Ignore gender/other/number -- note there may be more combinations in the future
        if selector_row[E_FEAT] == "other" or selector_row[E_FEAT] == "gender" or selector_row[E_FEAT] == "number" or selector_row[E_FEAT] == "number" or selector_row[E_FEAT] == "number+gender":
            continue
        for header in [E_SEL, E_SEL_TYPE, E_OV, E_FEAT]:
            row[header] = selector_row[header]
        if current_language != row[E_GLOT]:
            # yield only rows belonging to that type
            dummy = [
                l
                for l in language_rows
                if l[E_SEL_TYPE] in selector_for_this_type  # or l[E_REF] == "any"
            ]
            yield dummy
            language_rows = []
            language_rows.append(row)
            current_language = row[E_GLOT]
        else:
            language_rows.append(row)
    # once we exit the for loop, we still need to return the last language
    dummy = [
        l
        for l in language_rows
        if l[E_SEL_TYPE] in selector_for_this_type  # or l[E_REF] == "any"
    ]
    yield dummy


def write_intermediate_csv(
    metadata: str,
    outfile: str,
    types: t.List[str],
    locus_type: t.Optional[str] = None,
    birchallian: t.List[str] = ["exemplar", "non-exemplar"],
    write_mode: str = "a+",
    error_file: str = str(Path(__file__).parent.parent.parent / "outputs" / "alignment-errors.xlsx"),
    warning_file: str = str(Path(__file__).parent.parent.parent / "outputs" / "alignment-warnings.xlsx"), #
    output_errors_to_excel: bool = True,
):
    errors = SimpleContainer.from_excel(
        excel=error_file
    )
    warnings = SimpleContainer.from_excel(
        excel=warning_file,
    )
    dataset = pycldf.Dataset.from_metadata(metadata)
    with open(outfile, write_mode, encoding="utf8", newline="") as out:
        writer = DictWriter(out, INTERMEDIATE_HEADER)
        writer.writeheader()
        # columns, header = get_excel_header_to_column(ws)
        values = [INTERMEDIATE_HEADER[:]]
        all_languages = set()
        for r in dataset["languages.csv"]:
            all_languages.add(r[E_GLOT])
        intermediate_languages = set()
        for type in types:
            # yield language rows pre-selected based on selector type, further type distinction necessary as
            # noun and pronouns collapse into on group based on selector type
            for language_rows in yield_from_cldf(
                dataset=dataset, type=type, errors=errors
            ):
                rows = process_language_rows(
                    language_rows=language_rows,
                    type=type,
                    this_locus=locus_type,
                    birchallian=birchallian,
                    errors=errors,
                    warnings=warnings,
                )
                # check if there is only one any row, if so, copy the row and insert for every type a copy of the any row with any replaced by type
                any_indices = [i for i, r in enumerate(rows) if r[I_TYP] == "any"]
                if any_indices:
                    if type != "Verb":
                        local_types = set(
                            row[I_TYP] for row in rows if type in row[I_TYP]
                        ) - {"any"}
                    else:
                        local_types = set(row[I_TYP] for row in rows) - {"any"}
                    # copy the any rows
                    if local_types:
                        for this_type in local_types:
                            for i in any_indices:
                                any_row = rows[i]
                                row_copy = any_row.copy()
                                row_copy[I_TYP] = this_type
                                row_copy[I_ID] = re.sub(f"{row_copy[I_LOCUS]}_any", f"{row_copy[I_LOCUS]}_{this_type}", row_copy[I_ID])
                                rows.append(row_copy)
                        # remove anyrows
                        rows = [r for r in rows if r[I_TYP] != "any"]
                writer.writerows(rows)
                for row in rows:
                    intermediate_languages.add(row[E_GLOT])
                    values.append([row[H] for H in INTERMEDIATE_HEADER])
    if all_languages - intermediate_languages:
        errors.log(
            row=(
                type,
                "",
                "",
                "",
                f"Missing languages: {all_languages - intermediate_languages}",
            )
        )
    errors.to_excel()
    warnings.to_excel()
    return values


def process_language_rows(
    language_rows: t.List[t.Dict[str, t.Union[str, int, float]]],
    type: str,
    errors: SimpleContainer,
    warnings: SimpleContainer,
    this_locus: t.Optional[str] = None,
    birchallian: t.List[str] = ["exemplar", "non-exemplar"],
):
    global COUNTER
    out_rows = []
    for rows, birch, mono_pred, bi_pred, tam, domain in sub_select_rows(
        language_rows=language_rows,
        type=type,
        birchallian=birchallian,
        errors=errors,
        warnings=warnings,
    ):
        if not rows:
            continue
        elif type != "Verb":
            int_rows = create_intermediate_rows(
                language_rows=rows,
                type=type,
                this_prim=birch,
                this_locus=this_locus,
                this_mono=mono_pred,
                this_bi=bi_pred,
                this_tam=tam,
                this_misc=domain,
                errors=errors,
                warnings=warnings,
            )
            out_rows += int_rows
            continue
        # applied to all types, also verbs
        # TODO: Discuss this error, is it obsolete?
        elif "any" in [r[E_REF] for r in rows] and len([r[E_REF] for r in rows]) != len(
            rows
        ):
            continue
        # special cases verbs
        else:
            if len(rows) == 1:
                if (
                    rows[0][E_SEL_TYPE] == "indexing trigger"
                    and rows[0][E_SEL]
                    == "NULL_INDEXING"  # is "no indexing" for csv version
                ):
                    misc = rows[0][E_MISC]
                    int_rows = [
                        {
                            I_ID: f"{rows[0][E_GLOT]}_{type}_{rows[0][E_REF]}_{birch}{('_' + misc) if misc  else ''}",
                            I_GLOT: rows[0][E_GLOT],
                            I_LAN: rows[0][E_LAN],
                            I_LOCUS: type,
                            I_TYP: rows[0][E_REF],
                            I_PRIM: birch,
                            I_MISC: misc,
                            I_S: "no marking",
                            I_A: "no marking",
                            I_P: "no marking",
                            I_ALIG: "no marking",
                            I_SOURCE: "; ".join(sorted(
                                {s for row in rows for s in row[E_SOURCE]}
                            )),
                            I_COD: "; ".join(sorted(
                                {s.replace(" ", "") for row in rows for s in row[E_COD]}
                            )),
                        }
                    ]
                    COUNTER += 1
                else:
                    int_rows = create_intermediate_rows(
                        language_rows=rows,
                        type=type,
                        this_prim=birch,
                        this_locus=this_locus,
                        this_mono=mono_pred,
                        this_bi=bi_pred,
                        this_tam=tam,
                        this_misc=domain,
                        errors=errors,
                        warnings=warnings,
                    )
                #     errors.log(
                #         row=(
                #             type,
                #             rows[0][E_LAN],
                #             rows[0][E_GLOT],
                #             ", ".join(rows[0][E_COD]),
                #             f"Just one row without selector_type == “indexing trigger” & selector_label == “no indexing” with birchallian {sub_birch} and domain {domain}",
                #         )
                #     )
                #     continue
            else:
                int_rows = create_intermediate_rows(
                    language_rows=rows,
                    type=type,
                    this_prim=birch,
                    this_locus=this_locus,
                    this_mono=mono_pred,
                    this_bi=bi_pred,
                    this_tam=tam,
                    this_misc=domain,
                    errors=errors,
                    warnings=warnings,
                )
            out_rows += int_rows

    return out_rows


def sub_select_rows(
    language_rows: t.List[t.Dict[str, t.Union[str, int, float]]],
    type: str,
    birchallian: t.List[str],
    errors: SimpleContainer,
    warnings: SimpleContainer,
):
    # select all rows containing type
    if type == "Verb":
        # all rows are verbal alignments
        sub_rows = language_rows
    else:
        sub_rows = [
            row for row in language_rows if type in row[E_REF] or "any" in row[E_REF]
        ]
    # if no exemplar/non-exemplar (all any), then only go through Exemplar case "any";
    # if only non-exemplar present, assume exemplar (Exemplar = ['exemplar', 'non-exemplar'])
    # if only exemplar present, this is a definitional error
    if not any("non-exemplar" in r[E_BIR] for r in sub_rows):
        if not all("any" in r[E_BIR] for r in sub_rows):
            errors.log(
                row=(
                    type,
                    sub_rows[0][E_LAN],
                    sub_rows[0][E_GLOT],
                    ", ".join(sorted(sub_rows[0][E_COD])),
                    f"ERROR: Language {sub_rows[0][E_GLOT]} has no context marked <non-exemplar> but not all contexts are <any>. This should be impossible.",
                )
            )
    birchallian = ["exemplar", "all"]
    # filter for exemplar or non-exemplar
    for birch in birchallian:
        try:
            if birch == "all":
                matching_birchs = ["exemplar", "non-exemplar", "any"]
            else:
                matching_birchs = [birch, "any"]
            birch_rows = [
                row for row in sub_rows if row[E_BIR] in matching_birchs
            ]
            # if for this type no row in the selected rows contains the current birchallian value
            # don't add a row to the intermediate table
            if not birch_rows:
                continue
            # look for all values of monovalent predicate classes
            mono_preds = set(r[E_PRED] for r in [row for row in birch_rows if row[E_ROLE] == "S"]) - {"any"}
            # if there are no mono_preds, i.e. S is never indexed, hallucinate a default class
            if len(mono_preds) == 0:
                mono_preds = set(["default"])
            # look for all values in bivalent predicate classes
            bi_preds = set(r[E_PRED] for r in [row for row in birch_rows if row[E_ROLE] != "S"]) - {"any"}
            # if there are no bi_preds, i.e. A and P are never indexed (but S is), hallucinate a default class
            if len(bi_preds) == 0:
                bi_preds = set(["default"])
            # for each mono_pred, bi_pred, tam, and misc condition, subselect rows and yield for
            # for creation of intermediate rows
            for mono_pred in mono_preds:
                mono_pred_rows = [
                    row for row in birch_rows if ((row[E_PRED] == mono_pred or row[E_PRED] == "any") and row[E_ROLE] == "S") or
                                                 (row[E_ROLE] != "S")
                ]
                for bi_pred in bi_preds:
                    bi_pred_rows = [
                        row for row in mono_pred_rows if ((row[E_PRED] == bi_pred or row[E_PRED] == "any") and row[E_ROLE] != "S") or
                                                         (row[E_ROLE] == "S")
                    ]
                    tams = set(r[E_TAM] for r in bi_pred_rows)
                    if len(tams) > 1: # remove "any" from the loop - make "any" a wildcard - if there's more than 1 TAM
                        tams = tams - set(["any"])
                    for tam in tams:
                        matching_tams = [tam, "any"]
                        tam_rows = [
                            row for row in bi_pred_rows if row[E_TAM] in matching_tams
                        ]
                        domains = set(r[E_MISC] for r in tam_rows)
                        if domains != {"any"}:
                            domains = domains - {"any"}
                        for domain in domains:
                            selected_rows = [
                                row for row in tam_rows 
                                if row[E_MISC] == domain or row[E_MISC] == "any"
                            ]
                            yield selected_rows, birch, mono_pred, bi_pred, tam, domain
        except ValueError:
            yield [], birch, "", "", "", ""


def create_intermediate_rows(
    language_rows: t.List[t.Dict[str, t.Union[str, int, float]]],
    type: str,
    this_prim: str,
    errors: SimpleContainer,
    warnings: SimpleContainer,
    this_locus: t.Optional[str] = None,
    this_misc: str = None,
    this_mono: str = "Default",
    this_bi: str = "Default",
    this_tam: str = "any",
):
    global COUNTER
    out_rows = []
    # count noun types in reference
    if type != "Verb":
        types = set(
            row[E_REF]
            for row in language_rows
            if type in row[E_REF]
        ) - {"any"}
    else:
        types = set(row[E_REF] for row in language_rows) - {"any"}
    number_of_types = len(types)
    if number_of_types == 0:
        if set(row[E_REF] for row in language_rows) == {"any"}:
            out_row = {
                I_ID: f"{language_rows[0][E_GLOT]}_{type}_any_{this_prim}_{this_mono}_{this_bi}_{this_tam}{('_' + this_misc) if this_misc else ''}",
                I_GLOT: language_rows[0][E_GLOT],
                I_LAN: language_rows[0][E_LAN],
                I_LOCUS: type,
                I_TYP: "any",
                I_PRIM: this_prim,
                I_MONO: this_mono,
                I_BI: this_bi,
                I_TAM: this_tam,
                I_MISC: this_misc,
                I_SOURCE: "; ".join(sorted(
                    {s for row in language_rows for s in row[E_SOURCE]}
                )),
                I_COD: "; ".join(sorted(
                    {s.replace(" ", "") for row in language_rows for s in row[E_COD]}
                )),
            }
            COUNTER += 1
            for role, role_value in get_role_value(
                rows=language_rows,
                this_type="any",
                type=type,
                language=out_row[I_GLOT],
                errors=errors,
                warnings=warnings,
                birch=this_prim,
                language_rows= None,
            ):
                out_row[role] = role_value
            add_alignment(out_row=out_row, errors=errors, warnings=warnings)
            out_rows.append(out_row)
        else:
            # this is not necessarily an error. E.g. it could just be Pro and any
            # errors.log(
            #     row=(
            #         type,
            #         language_rows[0][E_LAN],
            #         language_rows[0][E_GLOT],
            #         ", ".join(language_rows[0][E_COD]),
            #         f"ERROR: 0 references found for {type} and not all references are any with birchallian {this_prim}. Reference is not taken into account if birchallian or domain is any",
            #     )
            # )
            pass
    else:
        all_types = [row[E_REF] for row in language_rows]
        for this_type in types:
            out_row = {
                I_ID: f"{language_rows[0][E_GLOT]}_{type}_{this_type}_{this_prim}_{this_mono}_{this_bi}_{this_tam}{('_' + this_misc) if this_misc  else ''}",
                I_GLOT: language_rows[0][E_GLOT],
                I_LAN: language_rows[0][E_LAN],
                I_LOCUS: type,
                I_TYP: this_type,
                I_PRIM: this_prim,
                I_MONO: this_mono,
                I_BI: this_bi,
                I_TAM: this_tam,
                I_MISC: this_misc,
                I_SOURCE: "; ".join(sorted(
                    {s for row in language_rows for s in row[E_SOURCE]}
                )),
                I_COD: "; ".join(sorted(
                    {s.replace(" ", "") for row in language_rows for s in row[E_COD]}
                )),
            }
            COUNTER += 1
            indices_this_type = {
                i for i, n in enumerate(all_types) if n == this_type or n == "any"
            }
            # filter for roles and create output rows
            sub_language_rows = [language_rows[j] for j in indices_this_type]
            for role, role_value in get_role_value(
                rows=sub_language_rows,
                this_type=this_type,
                type=type,
                language=out_row[I_GLOT],
                errors=errors,
                warnings=warnings,
                birch=this_prim,
                language_rows=language_rows,
            ):
                out_row[role] = collapse_role_value(
                    role_value, this_type, list(set(all_types))
                )
            add_alignment(out_row=out_row, errors=errors, warnings=warnings)
            out_rows.append(out_row)
    return out_rows


def get_role_value(
    rows: t.List[t.Dict[str, t.Union[str, int, float]]],
    this_type: str,
    type: str,
    language: str,
    errors: SimpleContainer,
    warnings: SimpleContainer,
    birch: str,
    language_rows: t.List[t.Dict[str, t.Union[str, int, float]]],
):
    for role in ["S", "A", "P"]:
        roles = [row[E_ROLE] for row in rows]
        # handle special case for pronouns
        if (
            roles == ["none"]
            and type == "Pro"
            and {row[E_SEL] for row in rows} == {"no independent pronouns"}
            and {row[E_OV] for row in rows} == {"zero"}
        ):
            yield role, "-"
            continue
        # handle special case for verbs
        elif (
            roles == ["none"]
            and type == "Verb"
            and {row[E_SEL] for row in rows} == {"NULL_INDEXING"}
            and {row[E_OV] for row in rows} == {"zero"}
        ):
            yield role, "-"
            continue
        roles_indices = {i for i, r in enumerate(roles) if r == role}
        # no matching roles
        if len(roles_indices) == 0:
            # find number of any references
            references_indices = {i for i, r in enumerate(rows) if r[E_REF] == "any"}
            matching_indices = roles_indices.intersection(references_indices)
            if len(matching_indices) == 0:
                if type == "Verb":
                    if language_rows:
                        if role in [r[E_ROLE] for r in language_rows]:
                            warnings.log(
                                row=(
                                    type,
                                    rows[0][E_LAN],
                                    language,
                                    ", ".join(sorted(rows[0][E_COD])),
                                    f"language has an inferred gap at reference: {this_type} and Role: {role} (INFERRED_NULL_zero)",
                                )
                            )
                            role_value = "INFERRED_NULL_zero"
                        else:
                            warnings.log(
                                row=(
                                    type,
                                    rows[0][E_LAN],
                                    language,
                                    ", ".join(sorted(rows[0][E_COD])),
                                    f"language has an inferred gap at reference: {this_type} and Role: {role} (ROLE_NOT_MARKED_zero)",
                                )
                            )
                            role_value = "ROLE_NOT_MARKED_zero"
                    else:
                        warnings.log(
                            row=(
                                type,
                                rows[0][E_LAN],
                                language,
                                ", ".join(sorted(rows[0][E_COD])),
                                f"language has an inferred gap at reference: {this_type} and Role: {role} (ROLE_NOT_MARKED_zero)",
                            )
                        )
                        role_value = "ROLE_NOT_MARKED_zero"
                    yield role, role_value
                    continue
                else:
                    if type == "Pro":
                        role_value = "INFERRED_NO_PRONOUN"
                    else:
                        role_value = f"ERROR in data. No Reference any and no role {role} for Birchallian case {birch}"
                    yield role, role_value
                    continue
            elif len(matching_indices) == 1:
                # co arg ref is any
                if "any" in [rows[i][E_CO] for i in matching_indices]:
                    role_value = f"{rows[matching_indices[0]][E_SEL]}_{rows[matching_indices[0]][E_OV]}"
                # co arg not any
                else:
                    role_value = "".join(sorted(
                        f"{rows[i][E_SEL]}_{rows[i][E_OV]}" for i in matching_indices
                    ))
            # more than one row with matching indices
            else:
                co_arg = [row[E_CO] for row in rows]
                if not len(set(co_arg[i] for i in matching_indices)) == len(
                    matching_indices
                ):
                    duplicated = defaultdict(lambda: 0)
                    dummy = []
                    for ele in [co_arg[i] for i in roles_indices]:
                        if ele not in dummy:
                            dummy.append(ele)
                        else:
                            duplicated[ele] += 1
                    ss = ", ".join(sorted([f"{k}(x{v})" for k, v in duplicated.items()]))
                    role_value = f"ERROR: Multiple rows with Reference {this_type} and Role {role} with identical co_arg roles: {ss}"
                else:
                    selectors = {rows[i][E_SEL] for i in roles_indices}
                    overts = {rows[i][E_OV] for i in roles_indices}

                    if len(selectors) == len(overts) == 1:
                        role_values = [f"{rows[0][E_SEL]}_{rows[0][E_OV]}"]
                    else:

                        role_values = [
                            f"{rows[i][E_SEL]}_{rows[i][E_OV]}_coarg:{rows[i][E_CO]}"
                            for i in roles_indices
                        ]
                    role_value = "; ".join(sorted(role_values))
            yield role, role_value
            continue
        # if there is a single entry AND ALSO nothing in the coarg field
        elif len(roles_indices) == 1 and (
            rows[list(roles_indices)[0]][E_CO] == "NA"
            or rows[list(roles_indices)[0]][E_CO] == "any"
        ):
            role_value = "".join(sorted(
                f"{rows[i][E_SEL]}_{rows[i][E_OV]}" for i in roles_indices
            ))
        # more than one row with matching roles
        else:
            co_arg = [rows[i][E_CO] for i in roles_indices]
            if type == "Verb":
                coargs_slots = {(rows[i][E_CO], rows[i][E_SLOT]) for i in roles_indices}
                # not as many unique (coarg, slots) as rows
                if len(roles_indices) != len(coargs_slots):
                    errors.log(
                        row=(
                            type,
                            rows[0][E_LAN],
                            language,
                            ", ".join(sorted(rows[0][E_COD])),
                            f"ERROR: Not as many unique pairs of (Co_arg, Slot) as matching rows with reference: {this_type} and role: {role}",
                        )
                    )
                    role_value = f"ERROR: Inconsistent slot information for {this_type} and role {role}"
                    yield role, role_value
                # as many (coargs, slots) as rows
                else:
                    if language_rows is None:
                        all_types = set(["any"])
                    else:
                        all_types = set([row[E_REF] for row in language_rows])
                        if all_types != set(["any"]):
                            all_types = all_types - set(["any"])
                    if "incl" in this_type:
                        this_person = "incl"
                    elif "2/3" in this_type: # we will pretend this is 3 for alignment purposes
                        this_person = "3"
                    else:
                        this_person = re.sub("[^0-9]", "", this_type)
                    possible_coargs = [x for x in all_types]
                    if this_person == "1" or this_person == "2" or this_person == "incl":
                        possible_coargs = [x for x in possible_coargs if "incl" not in x]
                    if this_person == "1" or this_person == "incl":
                        possible_coargs = [x for x in possible_coargs if "1" not in x]
                    if this_person == "2" or this_person == "incl":
                        possible_coargs = [x for x in possible_coargs if "2" not in x]
                    if "high" in this_type:
                        in_both_places = any(["high" in rows[i][E_CO] for i in roles_indices])
                        if not in_both_places:
                            possible_coargs = [x for x in possible_coargs if "high" not in x]
                    if "low" in this_type:
                        in_both_places = any(["low" in rows[i][E_CO] for i in roles_indices])
                        if not in_both_places:
                            possible_coargs = [x for x in possible_coargs if "low" not in x]
                    co_arg_slot_strings = []
                    co_arg_to_slot = defaultdict(list)
                    # if some co_arg is any and other coargs are not any, we need to blow up the any to all possible
                    any_and_more = "any" in [rows[i][E_CO] for i in roles_indices] and \
                                   not all([rows[i][E_CO] == "any" for i in roles_indices])
                    for i in roles_indices:
                        if any_and_more and rows[i][E_CO] == "any":
                            for t in possible_coargs:
                                co_arg_to_slot[t].append((rows[i][E_SLOT], i))
                        else:
                            co_arg_to_slot[rows[i][E_CO]].append((rows[i][E_SLOT], i))
                    #sort every key in co_arg_to_slot
                    for coarg in co_arg_to_slot.keys():
                        co_arg_to_slot[coarg].sort(key=lambda x: x[0])
                    #slot_list.sort(key=lambda x: x[0])
                    # if there is only one coarg
                    if len(co_arg_to_slot) == 1:
                        # if this co arg is 'any' or 'NA' -- don't print out _coarg
                        if list(co_arg_to_slot.keys())[0] == 'any' or list(co_arg_to_slot.keys())[0] == 'NA':
                            if len(list(co_arg_to_slot.values())[0]) == 1:  # suppress slot
                                co_arg_slot_strings.append(" && ".join(sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}" for t in list(co_arg_to_slot.values())[0]])))
                            else:
                                co_arg_slot_strings.append(" && ".join(sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}_slot:{t[0]}" for t in list(co_arg_to_slot.values())[0]])))
                            role_value = " ; ".join(sorted(co_arg_slot_strings))
                        # only one co arg and it is not any
                        else:
                            all_coargs_listed = len(co_arg_to_slot.keys()) == len(possible_coargs)
                            # if all slots are the same, don't write it
                            suppress_slot = True if \
                                all([len(slotperson) == 1 for slotperson in co_arg_to_slot.values()]) and \
                                len(set([slotperson[0][0] for slotperson in co_arg_to_slot.values()])) == 1 else False
                            for co_arg, slot_list in co_arg_to_slot.items():
                                slot_list.sort(key=lambda x: x[0])
                                if suppress_slot:
                                    co_arg_slot_strings.append(" && ".join(
                                        sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}" for t in
                                         sorted(slot_list)])) + f"_coarg:{co_arg}")
                                else:
                                    co_arg_slot_strings.append(" && ".join(
                                        sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}_slot:{t[0]}" for t in
                                         sorted(slot_list)])) + f"_coarg:{co_arg}")
                            role_value = " ; ".join(sorted(co_arg_slot_strings))
                            if not all_coargs_listed and not ('any' in co_arg_to_slot.keys()):
                                for ca in sorted(set(possible_coargs) - set(co_arg_to_slot.keys())):
                                    role_value += f" ; INFERRED_NULL_zero_coarg:{ca}"
                    else:
                        # verify that no slot associated with coarg 'any' is associated with another coarg, if so -> error
                        if "any" in co_arg_to_slot.keys():
                            other_slots = [s for k in co_arg_to_slot for s in co_arg_to_slot[k] if k != 'any']
                            for any_slot in co_arg_to_slot["any"]:
                                if any_slot in other_slots:
                                    errors.log(
                                        row=(
                                            this_type,
                                            rows[0][E_LAN],
                                            language,
                                            ", ".join(sorted(rows[0][E_COD])),
                                            f"ERROR: Inconsistent coarguments for reference {this_type} and role {role} in language: {rows[0][E_GLOT]}",
                                        )
                                    )
                                    role_value = f"ERROR: Inconsistent coarguments for reference {this_type} and role {role} in language: {rows[0][E_GLOT]}"
                                    yield role, role_value
                        if (len(co_arg_to_slot.keys()) > len(possible_coargs)):
                            warnings.log(
                                row=(
                                    this_type,
                                    rows[0][E_LAN],
                                    language,
                                    ", ".join(sorted(rows[0][E_COD])),
                                    f"WARNING: More possible coarguments than referential types found for reference {this_type} and role {role} in language: {rows[0][E_GLOT]}, this is probably due to one or more monoexponential plurals",
                                )
                            )
                        all_coargs_listed = len(co_arg_to_slot.keys()) == len(possible_coargs)
                        # if the result is entirely the same regardless, don't mark it
                        if all_coargs_listed:
                            selectors = set()
                            for val in co_arg_to_slot.values():
                                val.sort(key=lambda x: x[0])
                                if len(val) == 1: # don't add slot information
                                    selectors.add(f"{rows[val[0][1]][E_SEL]}_{rows[val[0][1]][E_OV]}")
                                else: # do add slot information
                                    selectors.add(" && ".join(sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}_slot:{t[0]}" for t in val])))
                            if len(selectors) == 1:
                                yield role, list(selectors)[0]
                                continue
                        # if all slots are the same, don't write it
                        suppress_slot = True if \
                            all([len(slotperson) == 1 for slotperson in co_arg_to_slot.values()]) and \
                            len(set([slotperson[0][0] for slotperson in co_arg_to_slot.values()])) == 1 else False
                        for co_arg, slot_list in co_arg_to_slot.items():
                            slot_list.sort(key=lambda x: x[0])
                            if suppress_slot:
                                co_arg_slot_strings.append(" && ".join(
                                    sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}" for t in
                                     sorted(slot_list)])) + f"_coarg:{co_arg}")
                            else:
                                co_arg_slot_strings.append(" && ".join(
                                    sorted([f"{rows[t[1]][E_SEL]}_{rows[t[1]][E_OV]}_slot:{t[0]}" for t in
                                     sorted(slot_list)])) + f"_coarg:{co_arg}")
                        role_value = " ; ".join(sorted(co_arg_slot_strings))
                        if not all_coargs_listed and not ('any' in co_arg_to_slot.keys()):
                            for ca in sorted(set(possible_coargs) - set(co_arg_to_slot.keys())):
                                role_value += f" ; INFERRED_NULL_zero_coarg:{ca}"
                    yield role, role_value
                    continue
            # check co arg references not for verbs
            # as many co arg references as rows with this role
            elif not len(set(co_arg)) == len(roles_indices):
                duplicated = defaultdict(lambda: 1)
                dummy = []
                for ele in co_arg:
                    if ele not in dummy:
                        dummy.append(ele)
                    else:
                        duplicated[ele] += 1
                ss = ", ".join(sorted([f"{k}(x{v})" for k, v in duplicated.items()]))
                errors.log(
                    row=(
                        type,
                        rows[0][E_LAN],
                        language,
                        ", ".join(sorted(rows[0][E_COD])),
                        f"ERROR: Multiple rows with Reference {this_type} and Role {role} with identical co_arg references {ss}.",
                    )
                )
                role_value = f"ERROR: Multiple rows with Reference {this_type} and Role {role} with identical co_arg roles."
            else:
                selectors = {rows[i][E_SEL] for i in roles_indices}
                overts = {rows[i][E_OV] for i in roles_indices}
                if len(selectors) == len(overts) == 1:
                    role_values = [f"{rows[0][E_SEL]}_{rows[0][E_OV]}"]
                else:
                    role_values = [
                        f"{rows[i][E_SEL]}_{rows[i][E_OV]}_coarg:{rows[i][E_CO]}"
                        for i in roles_indices
                    ]
                role_value = "; ".join(sorted(role_values))
        yield role, role_value


def collapse_role_value(role_value, this_type, all_types):
    if "coarg:" not in role_value:
        return role_value

    role_value_by_coargs = [x.strip() for x in role_value.split(";")]

    # remove this person from types array
    if "incl" in this_type:
        this_person = "incl"
    elif "2/3" in this_type: # we will pretend this is 3 for alignment purposes
        this_person = "3"
    else:
        this_person = re.sub("[^0-9]", "", this_type)
    possible_coargs = [x for x in all_types]
    if this_person == "1" or this_person == "2" or this_person == "incl":
        possible_coargs = [
            x
            for x in all_types
            if "incl" not in x and this_person not in x
        ]
    if this_person == "incl":
        possible_coargs = [x for x in all_types if "1" not in x and "2" not in x]

    if len(role_value_by_coargs) < len(possible_coargs):
        return role_value

    selector_and_overt = role_value_by_coargs[0].split("coarg:")[0]
    all_types_same = True
    for role_value_by_coarg in role_value_by_coargs:
        if role_value_by_coarg.split("coarg:")[0] != selector_and_overt:
            all_types_same = False
        possible_coargs = [
            x for x in possible_coargs if x != role_value_by_coarg.split("coarg:")[1]
        ]

    if all_types_same and len(possible_coargs) == 0:
        # If the coargs are exhaustive AND ALSO _slot information was in and there's 
        # only one slot, then remove "_slot" in addition to "_coarg"
        if "_slot" in role_value_by_coargs[0] and not "&&" in role_value_by_coargs[0]:
            return role_value_by_coargs[0].split("_slot")[0]
        return role_value_by_coargs[0].split("_coarg")[0]
    else:
        return role_value


def add_alignment(
    out_row: t.Dict[str, str], errors: SimpleContainer, warnings: SimpleContainer
):
    # fill the alignment column
    alignment = "ERROR: Unknown combination of S A P"

    original_a = out_row[I_A][:]
    original_s = out_row[I_S][:]
    original_p = out_row[I_P][:]

    # get rid of "slot" information
    out_row[I_S] = re.sub("_slot:[^_( )\&\&]+", "", out_row[I_S])
    out_row[I_A] = re.sub("_slot:[^_( )\&\&]+", "", out_row[I_A])
    out_row[I_P] = re.sub("_slot:[^_( )\&\&]+", "", out_row[I_P])

    # if errors in role, write error to alignment and exit
    if "ERROR" in out_row[I_A] or "ERROR" in out_row[I_P] or "ERROR" in out_row[I_S]:
        out_row[I_ALIG] = alignment
        return

    if (
        "_zero" in out_row[I_A]
        and "coarg:" not in out_row[I_A]
        and "_zero" in out_row[I_S]
        and "coarg:" not in out_row[I_S]
        and "_zero" in out_row[I_P]
        and "coarg:" not in out_row[I_P]
    ):
        if (
            "NO_PRONOUN" in out_row[I_A]
            and "NO_PRONOUN" in out_row[I_S]
            and "NO_PRONOUN" in out_row[I_P]
        ):
            alignment = "NA"
    # for purposes of comparison, remove zeros coordinated with an overt
    out_row[I_S] = re.sub("[^\&]*_zero[^&]*\&\&( )|( )\&\&[^&]*zero[^&]*","",out_row[I_S]);
    out_row[I_A] = re.sub("[^\&]*_zero[^&]*\&\&( )|( )\&\&[^&]*zero[^&]*","",out_row[I_A]);
    out_row[I_P] = re.sub("[^\&]*_zero[^&]*\&\&( )|( )\&\&[^&]*zero[^&]*","",out_row[I_P]);

    # replace complex strings with ZERO
    if (all(["_zero" in coarg and "_overt" not in coarg and coarg != "NO_PRONOUN_zero" for coarg in out_row[I_A].split(";")])) :
        out_row[I_A] = "ZERO"

    if (all(["_zero" in coarg and "_overt" not in coarg and coarg != "NO_PRONOUN_zero" for coarg in out_row[I_S].split(";")])) :
        out_row[I_S] = "ZERO"

    if (all(["_zero" in coarg and "_overt" not in coarg and coarg != "NO_PRONOUN_zero" for coarg in out_row[I_P].split(";")])) :
        out_row[I_P] = "ZERO"

    # elif ";" in out_row[I_A] or ";" in out_row[I_P] or ";" in out_row[I_S]:
    if (
        "coarg:" in out_row[I_A] or "coarg:" in out_row[I_P] or "coarg:" in out_row[I_S]
    ):
        alignment = "sensitive"
    elif out_row[I_A] == out_row[I_S] != out_row[I_P]:
        alignment = "accusative"
    elif out_row[I_A] == out_row[I_S] == out_row[I_P]:
        if "ZERO" in out_row[I_P]:
            alignment = "no marking"
        elif "overt" in out_row[I_P]:
            alignment = "overt neutral"
        elif "-" in out_row[I_P]:
            alignment = "NA"
        elif "?" in out_row[I_P]:
            alignment = "?"
    elif out_row[I_P] == out_row[I_A] != out_row[I_S]:
        alignment = "horizontal"
    elif out_row[I_P] == out_row[I_S] != out_row[I_A]:
        alignment = "ergative"
    # this must be the last one to check, otherwise if P==S!=A, below expression would also be true
    elif out_row[I_P] != out_row[I_A] != out_row[I_S]:
        alignment = "tripartite"


    out_row[I_ALIG] = alignment
    # restore original values
    out_row[I_A] = original_a
    out_row[I_S] = original_s
    out_row[I_P] = original_p


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-e", "--errors", help="Path to file where errors are written to. Default: ../../outputs/errors.xlsx", default=str(Path(__file__).parent.parent.parent / "outputs" / "errors.xlsx"), type=str,
    )
    parser.add_argument(
        "-w", "--warnings", help="Path to file where warnings are written to. Default: ../../outputs/warnings.xlsx", default=str(Path(__file__).parent.parent.parent / "outputs" / "errors.xlsx"), type=str,
    )
    parser.add_argument(
        "-o", "--output", help="Path to file where reference alignment is written to. Default: ../../cldf-alignment/generic-cldf/references.csv", default=str(Path(__file__).parent.parent.parent / "cldf-alignment" / "generic-cldf" / "references.csv"), type=str,
    )
    parser.add_argument(
        "-m", "--metadata", help="Path to cldf metadata. Default: ../../cldf-alignment/generic-cldf/metadata.json", default=str(Path(__file__).parent.parent.parent / "cldf-alignment" / "generic-cldf" / "metadata.json"), type=str,
    )
    args = parser.parse_args()
    
    values = write_intermediate_csv(
        outfile=args.output,
        types=["Verb", "Noun", "Pro"],
        metadata=args.metadata,
        error_file=args.errors,
        warning_file=args.warnings,
    )

