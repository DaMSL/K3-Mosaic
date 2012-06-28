open Testing
open K3
open K3Typechecker

let tests = group "all" [
    group "Constants" [
        case "Booleans" @: give_type (iparse "true") @=? TValue(canonical TBool);
        case "Integers" @: give_type (iparse "1") @=? TValue(canonical TInt);
        case "Floats" @: give_type (iparse "1.0") @=? TValue(canonical TFloat)
    ];
    group "Arithmetic" [
        group "Boolean" [
            case "Addition" @: give_type (iparse "false + false") @=? TValue(canonical TBool);
            case "Multiplication" @: give_type (iparse "true * false") @=? TValue(canonical TBool);
            case "Negation" @: give_type (iparse "-true") @=? TValue(canonical TBool)
        ];
        group "Integers" [
            case "Addition" @: give_type (iparse "1 + 1") @=? TValue(canonical TInt);
            case "Multiplication" @: give_type (iparse "1 * 1") @=? TValue(canonical TInt);
            case "Negation" @: give_type (iparse "-1") @=? TValue(canonical TInt)
        ];
        group "Floats" [
            case "Addition" @: give_type (iparse "1.0 + 1.0") @=? TValue(canonical TFloat);
            case "Multiplication" @: give_type (iparse "1.0 * 1.0") @=? TValue(canonical TFloat);
            case "Negation" @: give_type (iparse "-1.0") @=? TValue(canonical TFloat)
        ];
        group "Mixed" [
            case "Addition" @: give_type (iparse "1 + 1.0") @=? TValue(canonical TFloat);
            case "Multiplication" @: give_type (iparse "1.0 * 1") @=? TValue(canonical TFloat);
        ]
    ];
    group "Collection Construction" [
        group "Empty" [
            case "Set" @: give_type (iparse "{}") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TUnknown))));
            case "Bag" @: give_type (iparse "{||}") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TUnknown))));
            case "List" @: give_type (iparse "[]") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TUnknown))));
        ];
        group "Singleton" [
            case "Set" @: give_type (iparse "{0}") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TInt))));
            case "Bag" @: give_type (iparse "{|0|}") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TInt))));
            case "List" @: give_type (iparse "[0]") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
        ];
        group "Combine" [
            case "Set ++ Set" @: give_type (iparse "{0} ++ {1}") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TInt))));
            case "Set ++ Bag" @: give_type (iparse "{0} ++ {|1|}") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TInt))));
            case "Set ++ List" @: give_type (iparse "{0} ++ [1]") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
            case "Bag ++ Bag" @: give_type (iparse "{|0|} ++ {|1|}") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TInt))));
            case "Bag ++ List" @: give_type (iparse "{|0|} ++ [1]") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
            case "List ++ List" @: give_type (iparse "[0] ++ [1]") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
        ];
        group "Range" [
            case "IntSet" @: give_type (iparse "{0:1:20}") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TInt))));
            case "FloatBag" @: give_type (iparse "{|2.5:1:20|}") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TFloat))));
            case "FloatList" @: give_type (iparse "[1:2.5:100]") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TFloat))));
        ];
    ];
]

let _ = run_tests tests
