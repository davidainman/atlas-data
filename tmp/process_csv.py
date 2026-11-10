import segments
import re
import itertools
import attr
import pyclts
import pandas as pd

clts = pyclts.CLTS('/users/David/Documents/GitHub/clts/')
constructions_loc = "constructions.csv"
tokenizer = segments.Tokenizer()
test1 = "PSSR [t͡s]-PSSD-[kʼ]"
test2 = "PSSR-{m,im} PSSD-{a,i,u}"
test3 = "PSSR [∅]-PSSD-{hɑ,hu,ho,he,hi,ɑhɑ,uhu,oho,ehe,ihi,a,u,o,e,i}"
test4 = "PSSR {jin-irr,jina-rr,jina-∅,jin-∅,jini-∅} PSSD"
test5 = "PSSR [màà] PSSD.ToneChange"
test6 = "PSSD PSSR=[ᵐbe]"
test7 = "PSSR [ǁ’àn] {ha,hì} PSSD"
test8 = "PSSR-[qä] [kä]-PSSD"
test9 = "PSSR-{ba,pa} PSSD-[ṅ]"
test10 = "{l,lV}-PSSD PSSR"
bipa = clts.bipa


# recursively get combinations of a set of lists
# e.g. [[1,2]] --> [[1], [2]]
#      [[1,2],['a','b']] --> [[1,'a'], [1,'b'], [2,'a'], [2,'b']]
def get_combinations(list_of_lists):
    num_lists = len(list_of_lists)
    if num_lists == 1:
        return [[x] for x in list_of_lists[0]]
    ret_list = []
    for option in list_of_lists[0]:
        for combo in get_combinations(list_of_lists[1:]):
            ret_list.append([option] + [x for x in combo])
    return ret_list


def count_phonemes(
    formstring: str,
    unknowns: dict
):
    # expand {}s into all possible strings
    all_strings = [] # list of lists of strings
    if "{" in formstring:
        all_options = []
        constants = re.sub("\\{[^\\}]*\\}","*-*",formstring).split("*-*")
        for brackets in re.findall("\\{[^\\}]*\\}",formstring):
            options = []
            for option in brackets.strip('{').strip('}').split(','):
                options.append(option.strip())
            all_options.append(options)
        assert len(all_options) == len(constants) - 1
        if len(all_options) == 1:
            all_options = [[x] for x in all_options[0]]
        else:
            all_options = get_combinations(all_options)
        for brackets in all_options:
            new_str = constants[0]
            for i in range(0, len(brackets)):
                new_str += brackets[i] + constants[i+1]
            all_strings.append(new_str)
    else:
        all_strings.append(formstring)
    
    # for each possible form, remove PSSR(\.(.*)), PSSD(\.(.*)), [, ], -, =
    all_graphemes = [] # list of lists of graphemes
    for s in all_strings:
        repl = re.sub("PSSD(\\.[^\\s]*)?", " ", s)
        repl = re.sub("PSSR(\\.[^\\s]*)?", " ", repl)
        repl = re.sub("-|=|\\[|\\]", " ", repl)
        # normalize ejectives
        repl = re.sub("'|’", "ʼ", repl)
        graphemes = []
        for e in repl.strip().split():
            elem = e.strip()
            if elem != '\u2205': # don't add ∅
                graphemes.append(elem)
        all_graphemes.append(graphemes)
    
    # count the phonemic lengths of the graphemes
    all_lengths = [] # list of numbers
    for gs in all_graphemes:
        length = 0
        for g in gs:
            phonemes = segment_form(g, unknowns)
            length += len(phonemes)
        all_lengths.append(length)
    
    # calculate the average length
    avg = sum(all_lengths)/len(all_lengths)
    return avg


def segment_form(
    formstring: str,
    unknowns: set,
    system = bipa,
    split_diphthongs: bool = True,
    context_for_warnings: str = "",
) -> t.Iterable[pyclts.models.Symbol]:
    """Segment the form.
    
    This code is cannibalized from lexedata, originally developed by Gereon Kaiping
    https://github.com/lexedata/lexedata

    First, apply some pre-processing replacements. 
    Then, naïvely segment the form using the IPA tokenizer from the `segments`
    package. Check each returned segment to see whether it is valid according
    to CLTS's BIPA, and if not, try to fix some issues (in particular
    pre-aspirated or pre-nasalized consonants showing up as post-aspirated
    resp. post-nasalized vowels, which BIPA does not accept).

    >>> [str(x) for x in segment_form("iɾũndɨ", report=SegmentReport())]
    ['i', 'ɾ', 'ũ', 'n', 'd', 'ɨ']
    >>> [str(x) for x in segment_form("mokõi", report=SegmentReport())]
    ['m', 'o', 'k', 'õ', 'i']
    >>> segment_form("pan̥onoót͡síkoːʔú", report=SegmentReport())  # doctest: +ELLIPSIS
    [<pyclts.models.Consonant: voiceless bilabial stop consonant>, <pyclts.models.Vowel: unrounded open front vowel>, <pyclts.models.Consonant: devoiced voiced alveolar nasal consonant>, <pyclts.models.Vowel: rounded close-mid back vowel>, <pyclts.models.Consonant: voiced alveolar nasal consonant>, <pyclts.models.Vowel: rounded close-mid back vowel>, <pyclts.models.Vowel: rounded close-mid back ... vowel>, <pyclts.models.Consonant: voiceless alveolar sibilant affricate consonant>, <pyclts.models.Vowel: unrounded close front ... vowel>, <pyclts.models.Consonant: voiceless velar stop consonant>, <pyclts.models.Vowel: long rounded close-mid back vowel>, <pyclts.models.Consonant: voiceless glottal stop consonant>, <pyclts.models.Vowel: rounded close back ... vowel>]

    """
    
    # and with the syllable boundary marker '.', so we wrap it with special cases for those.
    raw_tokens = [
        system[s]
        for s in tokenizer(
            formstring,
            ipa=True,
        ).split()
    ]
    if system != bipa:
        for r in raw_tokens:
            if r.type == "unknownsound":
                print(
                    f"{context_for_warnings}Unknown sound encountered in {formstring:}"
                )
                unknowns.add(r)
    i = len(raw_tokens) - 1
    while i >= 0:
        if split_diphthongs and raw_tokens[i].type() == "diphthong":
            raw_tokens[i : i + 1] = [raw_tokens[i].from_sound, raw_tokens[i].to_sound]
            i += 1
            continue
        if raw_tokens[i].type() != "unknownsound":
            i -= 1
            continue
        if raw_tokens[i].source == "/":
            del raw_tokens[i]
            print(
                f"{context_for_warnings}Impossible sound '/' encountered in {formstring} – "
                f"You cannot use CLTS extended normalization "
                f"with this script. The slash was skipped and not included in the segments."
            )
            i -= 1
            continue
        grapheme = raw_tokens[i].grapheme
        if grapheme in {".", "-"}:
            del raw_tokens[i]
            i -= 1
            continue
        if grapheme == "ː":
            del raw_tokens[i]
            try:
                raw_tokens[i - 1] = bipa["long " + raw_tokens[i - 1].name]
            except TypeError:
                raw_tokens[i - 1].grapheme += "ː"
            i -= 1
            continue
        if grapheme in {"V", "C"}: # generic vowel, consonant
            i -= 1 # do nothing
            continue
        if grapheme.endswith("ⁿ") or grapheme.endswith("ᵐ") or grapheme.endswith("ᵑ"):
            if (
                i + 1 > len(raw_tokens) - 1
                or not hasattr(raw_tokens[i + 1], "preceding")
                or raw_tokens[i + 1].preceding is not None
            ):
                print(
                    f"{context_for_warnings}Unknown sound {raw_tokens[i]} encountered in {formstring}"
                )
                unknowns.add(grapheme)
                i -= 1
                continue
            raw_tokens[i + 1] = bipa["pre-nasalized " + raw_tokens[i + 1].name]
            raw_tokens[i] = bipa[grapheme[:-1]]
            continue
        if grapheme.endswith("ʰ"):
            if (
                i + 1 > len(raw_tokens) - 1
                or not hasattr(raw_tokens[i + 1], "preceding")
                or raw_tokens[i + 1].preceding is not None
            ):
                print(
                    f"{context_for_warnings}Unknown sound {raw_tokens[i]} encountered in {formstring}"
                )
                unknowns.add(grapheme)
                i -= 1
                continue
            raw_tokens[i + 1] = bipa["pre-aspirated " + raw_tokens[i + 1].name]
            raw_tokens[i] = bipa[grapheme[:-1]]
            continue
        print(
            f"{context_for_warnings}Unknown sound {raw_tokens[i]} encountered in {formstring}"
        )
        unknowns.add(raw_tokens[i].grapheme)
        i -= 1
    return raw_tokens


def add_counts_column():
    constructions = pd.read_csv(constructions_loc)
    unknowns = set()
    for index, row in constructions.iterrows():
        if pd.notna(row.Construction_Form_Coder) and row.Construction_Type != "POSSESSION" and \
          row.Construction_Type != "UNPOSSESSION" and row.Construction_Type != "SUPPLETION":
            raw_form = row.Construction_Form_Detailed
            form_length = count_phonemes(raw_form, unknowns)
            row.Construction_Form_Phone_Count = form_length
    print("Unknown segments:", str(unknowns))



unknowns = set()
for test in [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]:
    print(count_phonemes(test, unknowns))
print("Unknown segments:", str(unknowns))