//! Snapshot tests for PKL evaluation

use rpkl_parser::parse_module;
use rpkl_runtime::Evaluator;
use rpkl_stdlib::stdlib_registry;

fn eval_pkl(source: &str) -> String {
    let module = parse_module(source).expect("Failed to parse");
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    let result = evaluator.eval_module(&module).expect("Failed to evaluate");
    serde_json::to_string_pretty(&result).expect("Failed to serialize")
}

fn eval_pkl_result(source: &str) -> String {
    let module = match parse_module(source) {
        Ok(m) => m,
        Err(e) => return format!("Parse error: {}", e),
    };
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    match evaluator.eval_module(&module) {
        Ok(result) => serde_json::to_string_pretty(&result).expect("Failed to serialize"),
        Err(e) => format!("Eval error: {}", e),
    }
}

// =============================================================================
// Basic Types
// =============================================================================

#[test]
fn test_literals() {
    let source = r#"
        null_val = null
        bool_true = true
        bool_false = false
        int_val = 42
        float_val = 3.14
        string_val = "hello"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_interpolation() {
    let source = r#"
        name = "world"
        greeting = "Hello, \(name)!"
        nested = "Result: \(1 + 2)"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Arithmetic
// =============================================================================

#[test]
fn test_arithmetic() {
    let source = r#"
        add = 1 + 2
        sub = 10 - 3
        mul = 4 * 5
        div = 20 / 4
        int_div = 7 ~/ 2
        mod = 17 % 5
        pow = 2 ** 10
        precedence = 1 + 2 * 3
        parens = (1 + 2) * 3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_float_arithmetic() {
    let source = r#"
        add = 1.5 + 2.5
        mul = 2.5 * 4.0
        div = 10.0 / 4.0
        mixed = 1 + 2.5
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Comparisons and Logic
// =============================================================================

#[test]
fn test_comparisons() {
    let source = r#"
        eq = 1 == 1
        ne = 1 != 2
        lt = 1 < 2
        le = 2 <= 2
        gt = 3 > 2
        ge = 3 >= 3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_logical_operators() {
    let source = r#"
        and_tt = true && true
        and_tf = true && false
        or_tf = true || false
        or_ff = false || false
        not_t = !true
        not_f = !false
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_null_coalescing() {
    let source = r#"
        with_value = "value" ?? "default"
        with_null = null ?? "default"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Control Flow
// =============================================================================

#[test]
fn test_conditionals() {
    let source = r#"
        if_true = if (true) "yes" else "no"
        if_false = if (false) "yes" else "no"
        if_expr = if (1 < 2) "less" else "greater"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_let_bindings() {
    let source = r#"
        result = let (x = 10) let (y = 20) x + y
        nested = let (a = 1) let (b = a + 1) let (c = b + 1) c
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Objects
// =============================================================================

// TODO: Parser doesn't yet support nested object syntax without leading property names
// #[test]
// fn test_nested_objects() {
//     let source = r#"
//         outer = new {
//             inner = new {
//                 value = 42
//             }
//             name = "test"
//         }
//     "#;
//     insta::assert_snapshot!(eval_pkl(source));
// }

#[test]
fn test_simple_object() {
    let source = r#"
        name = "test"
        value = 42
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_listing() {
    let source = r#"
        items = new Listing {
            1
            2
            3
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// Note: Multiple Mapping entries require semicolons between them
#[test]
fn test_mapping_multiple_entries() {
    let source = r#"
        config = new Mapping {
            ["key1"] = "value1"; ["key2"] = "value2"; ["key3"] = 42
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_single_entry_mapping() {
    let source = r#"
        config = new Mapping { ["key1"] = "value1" }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Duration and DataSize
// =============================================================================

#[test]
fn test_duration() {
    let source = r#"
        ns = 100.ns
        ms = 500.ms
        s = 30.s
        min = 5.min
        h = 2.h
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_data_size() {
    let source = r#"
        bytes = 1024.b
        kb = 10.kb
        mb = 256.mb
        gb = 1.gb
        mib = 512.mib
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Lambda and Higher-Order Functions
// =============================================================================

#[test]
fn test_lambda() {
    let source = r#"
        double = (x) -> x * 2
        result = double.apply(21)
    "#;
    // Note: lambdas serialize as "<function>" in JSON
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_list_map() {
    let source = r#"
        numbers = new Listing { 1; 2; 3 }
        doubled = numbers.toList().map((x) -> x * 2)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_list_filter() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
            4
            5
        }
        evens = numbers.toList().filter((x) -> x % 2 == 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_fold() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
            4
            5
        }
        sum = numbers.toList().fold(0, (acc, x) -> acc + x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_any_every() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
            4
            5
        }
        hasEven = numbers.toList().any((x) -> x % 2 == 0)
        allPositive = numbers.toList().every((x) -> x > 0)
        allEven = numbers.toList().every((x) -> x % 2 == 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String Methods
// =============================================================================

#[test]
fn test_string_methods() {
    let source = r#"
        str = "Hello, World!"
        len = str.length
        upper = str.toUpperCase()
        lower = str.toLowerCase()
        contains = str.contains("World")
        starts = str.startsWith("Hello")
        ends = str.endsWith("!")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_manipulation() {
    let source = r#"
        original = "  hello world  "
        trimmed = original.trim()
        replaced = "hello".replaceAll("l", "L")
        split_result = "a,b,c".split(",")
        chars = "abc".chars()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int/Float Methods
// =============================================================================

#[test]
fn test_int_methods() {
    // Note: Avoid identifiers starting with keywords like 'is' as parser may confuse with type_op
    let source = r#"
        absVal = (-42).abs()
        signVal = (-42).sign()
        evenCheck = 4.isEven()
        oddCheck = 5.isOdd()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_float_methods() {
    let source = r#"
        f = 3.7
        ceil_val = f.ceil()
        floor_val = f.floor()
        round_val = f.round()
        sqrt_val = 16.0.sqrt()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List Methods
// =============================================================================

#[test]
fn test_list_basic_methods() {
    let source = r#"
        items = new Listing {
            "a"
            "b"
            "c"
        }
        list = items.toList()
        len = list.length
        first_item = list.first
        last_item = list.last
        reversed = list.reverse()
        joined = list.join(", ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_take_drop() {
    let source = r#"
        items = new Listing {
            1
            2
            3
            4
            5
        }
        list = items.toList()
        taken = list.take(3)
        dropped = list.drop(2)
        take_last = list.takeLast(2)
        drop_last = list.dropLast(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_contains() {
    let source = r#"
        items = new Listing {
            1
            2
            3
        }
        list = items.toList()
        has_2 = list.contains(2)
        has_5 = list.contains(5)
        idx = list.indexOf(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Complex Examples
// =============================================================================

#[test]
fn test_config_example() {
    let source = r#"
        name = "My Application"
        version = "1.0.0"
        port = 8080
        debug = true

        database {
            host = "localhost"
            port = 5432
            name = "myapp"
        }

        servers = new Listing {
            "server1.example.com"
            "server2.example.com"
        }

        settings {
            maxConnections = 100
            timeout = 30.s
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_computed_properties() {
    let source = r#"
        width = 10
        height = 20
        area = width * height
        perimeter = 2 * (width + height)
        description = "Rectangle: \(width)x\(height), area=\(area)"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// For/When Generators
// =============================================================================

#[test]
fn test_for_generator_basic() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
        }
        doubled = new Listing {
            for (n in numbers.toList()) {
                n * 2
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_for_generator_with_index() {
    let source = r#"
        items = new Listing {
            "a"
            "b"
            "c"
        }
        indexed = new Mapping {
            for (i, item in items.toList()) {
                [i] = item
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_when_generator_true() {
    let source = r#"
        debug = true
        config = new {
            name = "app"
            when (debug) {
                logLevel = "debug"
                verbose = true
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_when_generator_false() {
    let source = r#"
        debug = false
        config = new {
            name = "app"
            when (debug) {
                logLevel = "debug"
            } else {
                logLevel = "info"
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_nested_generators() {
    let source = r#"
        rows = new Listing { 1; 2 }
        cols = new Listing { 10; 20 }
        matrix = new Listing {
            for (r in rows.toList()) {
                for (c in cols.toList()) {
                    r + c
                }
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Classes
// =============================================================================

#[test]
fn test_class_basic() {
    let source = r#"
        class Person {
            name: String
            age: Int = 0
        }

        person1 = new Person {
            name = "Alice"
            age = 30
        }

        person2 = new Person {
            name = "Bob"
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_class_inheritance() {
    let source = r#"
        class Animal {
            name: String
            sound: String = "..."
        }

        class Dog extends Animal {
            sound = "woof"
            breed: String = "mixed"
        }

        animal = new Animal {
            name = "Generic"
        }

        dog = new Dog {
            name = "Rex"
            breed = "German Shepherd"
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_class_methods() {
    let source = r#"
        class Rectangle {
            width: Int
            height: Int

            function area() = width * height
            function perimeter() = 2 * (width + height)
        }

        rect = new Rectangle {
            width = 10
            height = 5
        }

        rectArea = rect.area()
        rectPerimeter = rect.perimeter()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_class_override_property() {
    let source = r#"
        class Config {
            host: String = "localhost"
            port: Int = 8080
            debug: Boolean = false
        }

        devConfig = new Config {
            debug = true
        }

        prodConfig = new Config {
            host = "prod.example.com"
            port = 443
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Imports
// =============================================================================

fn eval_pkl_file(path: &str) -> String {
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    match evaluator.eval_file(path) {
        Ok(result) => serde_json::to_string_pretty(&result).expect("Failed to serialize"),
        Err(e) => format!("Eval error: {}", e),
    }
}

#[test]
fn test_import_basic() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");
    let path = format!("{}/config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_import_aliased() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");
    let path = format!("{}/aliased.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_import_standalone() {
    // Test loading a module with no imports
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");
    let path = format!("{}/utils.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Module-level Methods
// =============================================================================

#[test]
fn test_module_level_function() {
    let source = r#"
        function double(x) = x * 2
        function add(a, b) = a + b

        result1 = double(21)
        result2 = add(10, 5)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_module_function_with_module_properties() {
    let source = r#"
        multiplier = 3

        function scale(x) = x * multiplier

        scaled = scale(7)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Type System - is operator
// =============================================================================

#[test]
fn test_is_basic_types() {
    let source = r#"
        int_is_int = 42 is Int
        int_is_string = 42 is String
        string_is_string = "hello" is String
        bool_is_bool = true is Boolean
        null_is_null = null is Null
        float_is_float = 3.14 is Float
        float_is_number = 3.14 is Number
        int_is_number = 42 is Number
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_nullable_types() {
    let source = r#"
        null_is_nullable_string = null is String?
        string_is_nullable_string = "hello" is String?
        int_is_nullable_int = 42 is Int?
        null_is_nullable_int = null is Int?
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_union_types() {
    let source = r#"
        int_is_int_or_string = 42 is Int|String
        string_is_int_or_string = "hello" is Int|String
        bool_is_int_or_string = true is Int|String
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_parameterized_list() {
    let source = r#"
        int_list = new Listing { 1; 2; 3 }
        string_list = new Listing { "a"; "b"; "c" }
        mixed_list = new Listing { 1; "two"; 3 }

        int_list_is_list = int_list.toList() is List
        int_list_is_list_int = int_list.toList() is List<Int>
        int_list_is_list_string = int_list.toList() is List<String>
        string_list_is_list_string = string_list.toList() is List<String>
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_constrained_types() {
    let source = r#"
        positive_num = 42
        negative_num = -10
        zero = 0

        positive_is_positive_int = positive_num is Int(this > 0)
        negative_is_positive_int = negative_num is Int(this > 0)
        zero_is_positive_int = zero is Int(this > 0)
        positive_is_non_negative = positive_num is Int(this >= 0)
        zero_is_non_negative = zero is Int(this >= 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_constrained_string() {
    let source = r#"
        short_str = "hi"
        long_str = "hello world"

        short_is_long = short_str is String(this.length > 5)
        long_is_long = long_str is String(this.length > 5)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_string_literal_type() {
    let source = r#"
        format_json = "json"
        format_yaml = "yaml"

        json_is_json = format_json is "json"
        yaml_is_json = format_yaml is "json"
        json_is_yaml = format_json is "yaml"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_any_type() {
    let source = r#"
        int_is_any = 42 is Any
        string_is_any = "hello" is Any
        null_is_any = null is Any
        list_is_any = (new Listing { 1 }).toList() is Any
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_collection_types() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        list = items.toList()

        list_is_list = list is List
        list_is_collection = list is Collection
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Type System - as operator
// =============================================================================

#[test]
fn test_as_compatible_types() {
    let source = r#"
        int_val = 42
        string_val = "hello"

        int_as_int = int_val as Int
        string_as_string = string_val as String
        int_as_any = int_val as Any
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_numeric_coercion() {
    let source = r#"
        int_val = 42
        float_val = 3.14

        int_as_number = int_val as Number
        float_as_number = float_val as Number
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_nullable() {
    let source = r#"
        int_val = 42
        null_val = null

        int_as_nullable_int = int_val as Int?
        null_as_nullable_int = null_val as Int?
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_type_error() {
    let source = r#"
        result = "hello" as Int
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_as_constraint_success() {
    let source = r#"
        positive = 42 as Int(this > 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_constraint_failure() {
    let source = r#"
        result = (-5) as Int(this > 0)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_as_parameterized_list() {
    let source = r#"
        int_list = new Listing { 1; 2; 3 }
        result = int_list.toList() as List<Int>
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_parameterized_list_failure() {
    let source = r#"
        int_list = new Listing { 1; 2; 3 }
        result = int_list.toList() as List<String>
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Type System - Combined tests
// =============================================================================

#[test]
fn test_type_check_in_conditional() {
    let source = r#"
        value = 42
        result = if (value is Int) "it's an int" else "not an int"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_type_with_function() {
    let source = r#"
        fn = (x) -> x * 2
        fn_is_function = fn is Function
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Module Inheritance - amends
// =============================================================================

#[test]
fn test_amends_basic() {
    // Test basic module amendment with property inheritance
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/dev_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_amends_nested_objects() {
    // Test that nested objects properly amend parent objects
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/prod_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_amends_chain() {
    // Test chained amendments (A amends B amends C)
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/staging_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_extends_basic() {
    // Test basic module extension (similar to amends but for type extension)
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/server_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}
