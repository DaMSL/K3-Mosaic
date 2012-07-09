open Testing
open K3
open K3Typechecker

let tests = group "all" [
    group "Constants" [
        case "Booleans" @: give_type (iparse "true") @=? TValue(canonical TBool);
        case "Integers" @: give_type (iparse "1") @=? TValue(canonical TInt);
        case "Floats" @: give_type (iparse "1.0") @=? TValue(canonical TFloat);
        case "Tuples" @: give_type (iparse "(1, 2)") @=?
            TValue(canonical (TTuple([canonical TInt; canonical TInt])))
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
    group "Comparisons" [
        case "Eq" @: give_type (iparse "0 == 1") @=? TValue(canonical TBool);
        case "Lt" @: give_type (iparse "0 < 1") @=? TValue(canonical TBool);
        case "Neq" @: give_type (iparse "0 != 1") @=? TValue(canonical TBool);
        case "Leq" @: give_type (iparse "0 <= 1") @=? TValue(canonical TBool);
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
    group "Control Flow" [
        group "Lambda" [
            case "Single Argument" @: give_type (iparse "\\x:int -> x + 1") @=?
                TFunction(canonical TInt, canonical TInt);
            case "Tuple Argument" @: give_type (iparse "\\(x:int, y:int) -> x + y") @=?
                TFunction(canonical (TTuple([canonical TInt; canonical TInt])), canonical TInt)
        ];
        group "Application" [
            case "Single Argument" @: give_type (iparse "(\\x:int -> x + 1)(1)") @=?  TValue(canonical TInt);
            case "Tuple Argument" @: give_type (iparse "(\\(x:int, y:int) -> x + y)(1, 2)") @=?
                TValue(canonical TInt)
        ];
        group "Iterate" [
            case "Simple Iterate" @: give_type (iparse "iterate(\\x:int -> (), [0; 1; 2])") @=?
                TValue(canonical TUnit);
        ];
        group "IfThenElse" [
            case "Simple IfThenElse" @: give_type (iparse "if true then () else ()") @=?
                TValue(canonical TUnit);
        ];
        group "Block" [
            case "Simple Block" @: give_type (iparse "do {(); (); 1}") @=? TValue(canonical TInt)
        ];
    ];
    group "Collection Transformers" [
        group "Map" [
            case "Map Succ" @: give_type (iparse "map(\\x:int -> x + 1, [0; 1; 2])") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
            case "Tuple Succ" @:
                give_type (iparse "map(\\(x:int, y:int) -> (x + 1, y + 1), [(0, 5); (1, 6); (2, 7)])") @=?
                TValue(canonical (TCollection(TList, contained_of (
                    canonical (TTuple([canonical TInt; canonical TInt]))
                ))));
            case "ZipWith (+)" @: give_type (iparse "map(\\(x:int, y:int) -> x + y, [(0, 5); (1, 6); (2, 7)])") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
        ];
        group "FilterMap" [
            case "Threshold Map" @: give_type
                (iparse "filtermap(\\x:int -> x > 1000, \\x:int -> x + 1, [0; 1; 2])") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TInt))));
        ];
        group "Flatten" [
            case "Flatten {{}}" @: give_type (iparse "flatten({{}})") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TUnknown))));
            case "Flatten {{||}}" @: give_type (iparse "flatten({{||}})") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TUnknown))));
            case "Flatten {[]}" @: give_type (iparse "flatten({[]})") @=?
                TValue(canonical (TCollection(TSet, contained_of (canonical TUnknown))));
            case "Flatten {|{}|}" @: give_type (iparse "flatten({|{}|})") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TUnknown))));
            case "Flatten {|{||}|}" @: give_type (iparse "flatten({|{||}|})") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TUnknown))));
            case "Flatten {|[]|}" @: give_type (iparse "flatten({|[]|})") @=?
                TValue(canonical (TCollection(TBag, contained_of (canonical TUnknown))));
            case "Flatten [{}]" @: give_type (iparse "flatten([{}])") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TUnknown))));
            case "Flatten [{||}]" @: give_type (iparse "flatten([{||}])") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TUnknown))));
            case "Flatten [[]]" @: give_type (iparse "flatten([[]])") @=?
                TValue(canonical (TCollection(TList, contained_of (canonical TUnknown))));
        ];
        group "Aggregate" [
            case "Simple Aggregation" @: give_type (iparse "fold(\\(z:int, a:int) -> z + a, 0, [1; 2; 3])") @=?
                TValue(canonical TInt)
        ];
        group "GroupByAggregate" [
            case "Simple GroupBy Aggregation" @:
                give_type (iparse "groupby(\\g:int -> g + 1, \\(z:int, a:int) -> z + a, 0, [1; 2; 3])") @=?
                TValue(canonical (TCollection(TList,
                    contained_of (canonical (TTuple([canonical TInt; canonical TInt])))
                )))
        ];
        group "Sort" [
            case "Simple Sort" @: give_type (iparse "sort([2; 3; 1], \\(a:int, b:int) -> (a < b))") @=?
                TValue(canonical (TCollection(TList, contained_of @: canonical TInt)))
        ];
        group "Access" [
            case "Single Element Slice" @: give_type (iparse "[1; 2; 3][_]") @=?
                TValue(canonical (TCollection(TList, contained_of @: canonical TInt)));
            case "Multi Element Slice" @: give_type (iparse "[(1, 2); (3, 4); (5, 6)][1, _]") @=?
                TValue(canonical (TCollection(TList,
                    contained_of (canonical (TTuple([canonical TInt; canonical TInt])))
                )));
            case "Peek" @: give_type (iparse "peek([1; 2; 3])") @=? TValue(contained_of @: canonical TInt)
        ];
        group "Collection Mutation" [
            case "Insert" @: give_type (iparse "insert([1; 2; 3], 5)") @=? TValue(canonical TUnit);
            case "Update" @: give_type (iparse "update([1; 2; 3], 1, 5)") @=? TValue(canonical TUnit);
            case "Delete" @: give_type (iparse "delete([1; 2; 3], 1)") @=? TValue(canonical TUnit);
        ];
    ];
]

let _ = run_tests tests
