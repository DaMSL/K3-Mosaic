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
]

let _ = run_tests tests
