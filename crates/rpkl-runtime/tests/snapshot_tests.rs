//! Snapshot tests for PKL evaluation

use rpkl_parser::parse_module;
use rpkl_runtime::Evaluator;
use rpkl_stdlib::stdlib_registry;

fn eval_pkl(source: &str) -> String {
    let module = parse_module(source).expect("Failed to parse");
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    let result = evaluator.eval_module(&module).expect("Failed to evaluate");
    evaluator.force_value(&result).expect("Failed to force");
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
        Ok(result) => {
            if let Err(e) = evaluator.force_value(&result) {
                return format!("Force error: {}", e);
            }
            serde_json::to_string_pretty(&result).expect("Failed to serialize")
        }
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
    // Note: abs, sign, isEven, isOdd are properties, not methods
    let source = r#"
        absVal = (-42).abs
        signVal = (-42).sign
        evenCheck = 4.isEven
        oddCheck = 5.isOdd
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
        Ok(result) => {
            if let Err(e) = evaluator.force_value(&result) {
                return format!("Force error: {}", e);
            }
            serde_json::to_string_pretty(&result).expect("Failed to serialize")
        }
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

// =============================================================================
// Amends Scope Tests - Regression tests for scope handling in module amendments
// =============================================================================

#[test]
fn test_amends_inherited_property_refs_child_values() {
    // Test that inherited properties can reference the child's overridden values
    // This was a bug where inherited output.pkg_name = package.name would fail
    // because package was Null (from parent) instead of using child's package
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/amends_scope");
    let path = format!("{}/child_with_ref.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_amends_typed_null_property() {
    // Test that amending a typed null property (e.g., about: About?) creates
    // a properly typed object with class defaults applied
    // This was a bug where a Dynamic object was created instead
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/amends_scope");
    let path = format!("{}/child_with_typed_null.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_class_method_accesses_module_imports() {
    // Test that class methods can access imports/values from their defining module
    // This was a bug where class methods captured the wrong scope
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/amends_scope");
    let path = format!("{}/use_imported_class.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Comprehensive Tests - Ported from apple/pkl upstream
// These tests are marked #[ignore] until the required features are implemented.
// Run with `cargo test -- --ignored` to check progress.
// =============================================================================

#[test]
fn test_upstream_generators_comprehensive() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/generators_comprehensive.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_string_methods() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/string_methods.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_list_methods() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/list_methods.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_classes_inheritance() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/classes_inheritance.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_type_operators() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/type_operators.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_numeric_methods() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/numeric_methods.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_realworld_config() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/realworld_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_when_generator() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/when_generator.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Multiline Strings
// =============================================================================

#[test]
fn test_multiline_string() {
    let source = r#"
        text = """
            Hello,
            World!
            """
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_multiline_string_interpolation() {
    let source = r#"
        name = "PKL"
        text = """
            Hello, \(name)!
            Version \(1 + 2)
            """
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Escape Sequences
// =============================================================================

#[test]
fn test_escape_sequences() {
    let source = r#"
        tab = "a\tb"
        newline = "a\nb"
        backslash = "a\\b"
        quote = "a\"b"
        null_char = "a\0b"
        unicode = "\u{41}\u{42}\u{43}"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Numeric Literal Formats
// =============================================================================

#[test]
fn test_hex_literals() {
    let source = r#"
        hex1 = 0xFF
        hex2 = 0x10
        hex3 = 0xDEAD
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_binary_literals() {
    let source = r#"
        bin1 = 0b1010
        bin2 = 0b11111111
        bin3 = 0b0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_octal_literals() {
    let source = r#"
        oct1 = 0o77
        oct2 = 0o10
        oct3 = 0o755
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_float_exponents() {
    let source = r#"
        e1 = 1e3
        e2 = 2.5e2
        e3 = 1.5e-3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Pipe Operator
// =============================================================================

#[test]
fn test_pipe_operator() {
    let source = r#"
        double = (x) -> x * 2
        add1 = (x) -> x + 1
        result = 5 |> double
        chained = 5 |> double |> add1
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Non-null Assertion
// =============================================================================

#[test]
fn test_non_null_assertion_success() {
    let source = r#"
        value = "hello"
        result = value!!
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_non_null_assertion_failure() {
    let source = r#"
        value = null
        result = value!!
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Optional Member Access
// =============================================================================

#[test]
fn test_optional_member_access() {
    let source = r#"
        name = "hello"
        len_of_value = name?.length
        len_of_null = null?.length
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Throw Expression
// =============================================================================

#[test]
fn test_throw_expression() {
    let source = r#"
        result = throw("Something went wrong")
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_throw_conditional() {
    let source = r#"
        x = -1
        result = if (x < 0) throw("x must be non-negative") else x
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Boolean Methods
// =============================================================================

#[test]
fn test_boolean_xor() {
    let source = r#"
        tt = true.xor(true)
        tf = true.xor(false)
        ft = false.xor(true)
        ff = false.xor(false)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_boolean_implies() {
    let source = r#"
        tt = true.implies(true)
        tf = true.implies(false)
        ft = false.implies(true)
        ff = false.implies(false)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int Properties (extended)
// =============================================================================

#[test]
fn test_int_properties_extended() {
    let source = r#"
        pos = 42
        neg = -10
        zero = 0

        pos_is_positive = pos.isPositive
        pos_is_negative = pos.isNegative
        pos_is_zero = pos.isZero
        neg_is_positive = neg.isPositive
        neg_is_negative = neg.isNegative
        zero_is_zero = zero.isZero
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Float Properties (extended)
// =============================================================================

#[test]
fn test_float_properties_extended() {
    let source = r#"
        f = 3.14
        is_finite = f.isFinite
        is_infinite = f.isInfinite
        is_nan = f.isNaN
        is_positive = f.isPositive
        is_negative = f.isNegative
        is_zero = f.isZero
        abs_val = (-3.14).abs
        sign_val = (-3.14).sign
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int Methods (extended)
// =============================================================================

#[test]
fn test_int_to_radix_string() {
    let source = r#"
        bin = 255.toRadixString(2)
        oct = 255.toRadixString(8)
        hex = 255.toRadixString(16)
        dec = 255.toRadixString(10)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_int_is_between() {
    let source = r#"
        in_range = 5.isBetween(1, 10)
        at_lower = 1.isBetween(1, 10)
        at_upper = 10.isBetween(1, 10)
        below = 0.isBetween(1, 10)
        above = 11.isBetween(1, 10)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_int_bitwise_operations() {
    let source = r#"
        shl = 1.shl(4)
        shr = 16.shr(2)
        bit_and = 0xFF.and(0x0F)
        bit_or = 0xF0.or(0x0F)
        bit_xor = 0xFF.xor(0x0F)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Duration Arithmetic
// =============================================================================

#[test]
fn test_duration_arithmetic() {
    let source = r#"
        sum = 5.s + 3.s
        diff = 10.s - 4.s
        mixed = 1.min + 30.s
        comparison = 60.s == 1.min
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_duration_properties() {
    let source = r#"
        d = 5.s
        val = d.value
        unit = d.unit
        is_pos = d.isPositive
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// DataSize Arithmetic
// =============================================================================

#[test]
fn test_datasize_arithmetic() {
    let source = r#"
        sum = 512.mb + 512.mb
        comparison = 1024.mb == 1.gb
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_datasize_properties() {
    let source = r#"
        d = 1024.kb
        val = d.value
        unit = d.unit
        is_pos = d.isPositive
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Subscript Access
// =============================================================================

#[test]
fn test_subscript_list() {
    let source = r#"
        items = new Listing { "a"; "b"; "c" }
        list = items.toList()
        first = list[0]
        last = list[2]
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_subscript_map() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        a_val = m["a"]
        c_val = m["c"]
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String Properties
// =============================================================================

#[test]
fn test_string_properties() {
    let source = r#"
        s = "hello"
        empty = ""
        blank = "   "

        s_is_empty = s.isEmpty
        s_is_not_empty = s.isNotEmpty
        s_is_blank = s.isBlank
        s_is_not_blank = s.isNotBlank
        s_last_index = s.lastIndex

        empty_is_empty = empty.isEmpty
        empty_is_blank = empty.isBlank
        blank_is_empty = blank.isEmpty
        blank_is_blank = blank.isBlank
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Additional String Methods
// =============================================================================

#[test]
fn test_string_pad_methods() {
    let source = r#"
        padded_start = "42".padStart(5, "0")
        padded_end = "hi".padEnd(5, ".")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_case_methods() {
    let source = r#"
        cap = "hello".capitalize()
        decap = "Hello".decapitalize()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_take_drop_while() {
    let source = r#"
        taken = "abcdef".take(3)
        dropped = "abcdef".drop(3)
        take_last = "abcdef".takeLast(3)
        drop_last = "abcdef".dropLast(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_index_of() {
    let source = r#"
        idx = "hello world".indexOf("world")
        last_idx = "hello hello".lastIndexOf("hello")
        not_found = "hello".indexOf("xyz")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_repeat() {
    let source = r#"
        repeated = "ab".repeat(3)
        empty = "ab".repeat(0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Map Methods and Properties
// =============================================================================

#[test]
fn test_map_methods() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        has_a = m.containsKey("a")
        has_z = m.containsKey("z")
        get_b = m.getOrNull("b")
        get_z = m.getOrNull("z")
        len = m.length
        empty = m.isEmpty
        keys = m.keys
        values = m.values
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Set Type
// =============================================================================

#[test]
fn test_set_creation() {
    let source = r#"
        s = Set(1, 2, 3, 2, 1)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// IntSeq Type
// =============================================================================

#[test]
fn test_intseq() {
    let source = r#"
        seq = IntSeq(0, 5, 1)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Pair Type
// =============================================================================

#[test]
fn test_pair() {
    let source = r#"
        p = Pair("key", 42)
        k = p.first
        v = p.second
        k2 = p.key
        v2 = p.value
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Listing Methods (direct on Listing objects)
// =============================================================================

#[test]
fn test_listing_properties() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        empty = new Listing {}
        len = items.length
        is_empty = items.isEmpty
        empty_is_empty = empty.isEmpty
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_listing_join() {
    let source = r#"
        items = new Listing { "a"; "b"; "c" }
        joined = items.join(", ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Mapping Methods
// =============================================================================

#[test]
fn test_mapping_methods() {
    let source = r#"
        m = new Mapping {
            ["a"] = 1; ["b"] = 2; ["c"] = 3
        }
        len = m.length
        is_empty = m.isEmpty
        has_a = m.containsKey("a")
        has_z = m.containsKey("z")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Advanced List Methods
// =============================================================================

#[test]
fn test_list_sort() {
    let source = r#"
        nums = new Listing { 3; 1; 4; 1; 5 }
        sorted = nums.toList().sort()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_flat_map() {
    let source = r#"
        nested = new Listing { 1; 2; 3 }
        result = nested.toList().flatMap((x) -> List(x, x * 10))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_zip() {
    let source = r#"
        a = new Listing { 1; 2; 3 }
        b = new Listing { "a"; "b"; "c" }
        zipped = a.toList().zip(b.toList())
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_list_map_indexed() {
    let source = r#"
        items = new Listing { "a"; "b"; "c" }
        result = items.toList().mapIndexed((i, v) -> "\(i):\(v)")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_filter_indexed() {
    let source = r#"
        items = new Listing { 10; 20; 30; 40; 50 }
        evens = items.toList().filterIndexed((i, v) -> i % 2 == 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_fold_indexed() {
    let source = r#"
        items = new Listing { 10; 20; 30 }
        result = items.toList().foldIndexed(0, (i, acc, v) -> acc + i * v)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_find_operations() {
    let source = r#"
        items = new Listing { 1; 2; 3; 4; 5 }
        list = items.toList()
        found = list.findOrNull((x) -> x > 3)
        not_found = list.findOrNull((x) -> x > 10)
        count = list.count((x) -> x % 2 == 0)
        find_index = list.findIndex((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_add_replace() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        list = items.toList()
        added = list.add(4)
        replaced = list.replace(1, 99)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_distinct_flatten() {
    let source = r#"
        dup_items = new Listing { 1; 2; 2; 3; 3; 3 }
        distinct = dup_items.toList().distinct()

        nested = List(List(1, 2), List(3, 4))
        flat = nested.flatten()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_min_max() {
    let source = r#"
        items = new Listing { 3; 1; 4; 1; 5; 9 }
        list = items.toList()
        min_val = list.min()
        max_val = list.max()
        min_or_null = list.minOrNull()
        max_or_null = list.maxOrNull()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_single() {
    let source = r#"
        one = List(42)
        single_val = one.single()
        many = List(1, 2)
        single_or_null = many.singleOrNull()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_get_or_null() {
    let source = r#"
        items = List(10, 20, 30)
        valid = items.getOrNull(1)
        invalid = items.getOrNull(99)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_take_drop_last() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        taken = items.take(3)
        dropped = items.drop(2)
        take_last = items.takeLast(2)
        drop_last = items.dropLast(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_last_index_of() {
    let source = r#"
        items = List(1, 2, 3, 2, 1)
        idx = items.lastIndexOf(2)
        not_found = items.lastIndexOf(99)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_repeat() {
    let source = r#"
        items = List(1, 2)
        repeated = items.repeat(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_split() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        result = items.split(3)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Local and Hidden Modifiers
// =============================================================================

#[test]
fn test_local_property() {
    let source = r#"
        local helper = 10
        result = helper * 2
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_hidden_property() {
    let source = r#"
        hidden secret = "classified"
        visible = "public"
        uses_hidden = secret
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Backtick Identifiers
// =============================================================================

#[test]
fn test_backtick_identifier() {
    let source = r#"
        `for` = "value"
        result = `for`
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// This/Outer in Objects
// =============================================================================

#[test]
fn test_this_in_object() {
    let source = r#"
        class Config {
            host: String = "localhost"
            port: Int = 8080
            function url() = "http://\(this.host):\(this.port)"
        }

        config = new Config {}
        url = config.url()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Object Amendment
// =============================================================================

#[test]
fn test_object_amendment() {
    let source = r#"
        class Server {
            host: String = "localhost"
            port: Int = 8080
        }

        base = new Server {}
        amended = (base) { port = 9090 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Spread Operator
// =============================================================================

#[test]
fn test_spread_in_listing() {
    let source = r#"
        first = new Listing { 1; 2 }
        second = new Listing { 3; 4 }
        combined = new Listing {
            ...first
            ...second
        }
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// TypeAlias
// =============================================================================

#[test]
fn test_typealias() {
    let source = r#"
        typealias StringList = List<String>

        items = new Listing { "a"; "b" }
        list = items.toList()
        check = list is StringList
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Abstract Classes
// =============================================================================

#[test]
fn test_abstract_class() {
    let source = r#"
        abstract class Shape {
            name: String
        }

        class Circle extends Shape {
            name = "Circle"
            radius: Float
        }

        c = new Circle { radius = 5.0 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Open Classes
// =============================================================================

#[test]
fn test_open_class() {
    let source = r#"
        open class Base {
            x: Int = 0
        }

        class Child extends Base {
            y: Int = 1
        }

        obj = new Child { x = 10; y = 20 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Class with Methods using module properties
// =============================================================================

#[test]
fn test_class_function_with_params() {
    let source = r#"
        class Calculator {
            function add(a, b) = a + b
            function multiply(a, b) = a * b
        }

        calc = new Calculator {}
        sum = calc.add(3, 4)
        product = calc.multiply(5, 6)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// For Generator in Mapping
// =============================================================================

#[test]
fn test_for_generator_in_mapping() {
    let source = r#"
        names = new Listing { "alice"; "bob"; "carol" }
        nameMap = new Mapping {
            for (name in names.toList()) {
                [name] = name.length
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Nested Object Properties
// =============================================================================

#[test]
fn test_nested_object_access() {
    let source = r#"
        config {
            database {
                host = "localhost"
                port = 5432
            }
            cache {
                ttl = 300.s
            }
        }
        db_host = config.database.host
        cache_ttl = config.cache.ttl
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Complex String Interpolation
// =============================================================================

#[test]
fn test_complex_string_interpolation() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        msg = "Count: \(items.toList().length), First: \(items.toList()[0])"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Empty Collections
// =============================================================================

#[test]
fn test_empty_collections() {
    let source = r#"
        empty_listing = new Listing {}
        empty_mapping = new Mapping {}
        empty_list = List()
        empty_map = Map()
        empty_set = Set()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Chained Method Calls
// =============================================================================

#[test]
fn test_chained_method_calls() {
    let source = r#"
        result = "Hello, World!"
            .toLowerCase()
            .replaceAll(",", "")
            .replaceAll("!", "")
            .split(" ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int/Float Conversions
// =============================================================================

#[test]
fn test_numeric_conversions() {
    let source = r#"
        int_val = 42
        float_val = 3.14

        int_to_float = int_val.toFloat()
        float_to_int = float_val.toInt()
        int_to_int = int_val.toInt()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// toString Method
// =============================================================================

#[test]
fn test_to_string_method() {
    let source = r#"
        int_str = 42.toString()
        float_str = 3.14.toString()
        bool_str = true.toString()
        null_str = null.toString()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List Properties
// =============================================================================

#[test]
fn test_list_properties() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        len = items.length
        is_empty = items.isEmpty
        is_not_empty = items.isNotEmpty
        first = items.first
        last = items.last
        rest = items.rest
        last_index = items.lastIndex
        is_distinct = items.isDistinct
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Map Properties (inline)
// =============================================================================

#[test]
fn test_map_properties() {
    let source = r#"
        m = Map("x", 1, "y", 2)
        len = m.length
        is_empty = m.isEmpty
        keys = m.keys
        values = m.values
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Float Math Methods
// =============================================================================

#[test]
fn test_float_math_methods() {
    let source = r#"
        sqrt_val = 25.0.sqrt()
        cbrt_val = 27.0.cbrt()
        ceil_val = 2.3.ceil()
        floor_val = 2.7.floor()
        round_val = 2.5.round()
        truncate_val = 2.9.truncate()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String Conversion Methods
// =============================================================================

#[test]
fn test_string_conversion_methods() {
    let source = r#"
        int_from_str = "42".toInt()
        float_from_str = "3.14".toFloat()
        bool_from_str = "true".toBoolean()
        int_or_null = "abc".toIntOrNull()
        float_or_null = "xyz".toFloatOrNull()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Multiple Class Hierarchy
// =============================================================================

#[test]
fn test_deep_class_hierarchy() {
    let source = r#"
        class A {
            x: Int = 1
        }
        class B extends A {
            y: Int = 2
        }
        class C extends B {
            z: Int = 3
        }

        obj = new C { x = 10 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Class with Default Values and Overrides
// =============================================================================

#[test]
fn test_class_defaults_and_overrides() {
    let source = r#"
        class Endpoint {
            method: String = "GET"
            path: String
            timeout: Duration = 30.s
        }

        health = new Endpoint {
            path = "/health"
        }

        slow = new Endpoint {
            method = "POST"
            path = "/upload"
            timeout = 120.s
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Listing with Typed Elements
// =============================================================================

#[test]
fn test_listing_with_strings() {
    let source = r#"
        class Tag {
            name: String
            color: String = "gray"
        }

        tags = new Listing {
            new Tag { name = "urgent"; color = "red" }
            new Tag { name = "low" }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Mapping with Complex Keys/Values
// =============================================================================

#[test]
fn test_mapping_with_computed_keys() {
    let source = r#"
        prefix = "key"
        m = new Mapping {
            ["\(prefix)_1"] = 10
            ["\(prefix)_2"] = 20
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// When Generator with Else
// =============================================================================

#[test]
fn test_when_else_generator() {
    let source = r#"
        isDev = false
        config = new {
            name = "app"
            when (isDev) {
                logLevel = "debug"
                verboseErrors = true
            } else {
                logLevel = "warn"
                verboseErrors = false
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Multiple When Generators
// =============================================================================

#[test]
fn test_multiple_when_generators() {
    let source = r#"
        enableCache = true
        enableLogging = false

        config = new {
            name = "service"
            when (enableCache) {
                cacheSize = 1024
            }
            when (enableLogging) {
                logFile = "/var/log/app.log"
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Negation of Expressions
// =============================================================================

#[test]
fn test_unary_negation() {
    let source = r#"
        a = 42
        neg_a = -a
        neg_lit = -10
        double_neg = -(-5)
        not_true = !true
        not_expr = !(1 > 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Complex Conditional Chains
// =============================================================================

#[test]
fn test_nested_conditionals() {
    let source = r#"
        x = 15
        result = if (x > 20) "high" else if (x > 10) "medium" else "low"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Let with Complex Expressions
// =============================================================================

#[test]
fn test_let_complex() {
    let source = r#"
        items = new Listing { 1; 2; 3; 4; 5 }
        result = let (list = items.toList()) let (sum = list.fold(0, (a, b) -> a + b)) sum
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int compareTo
// =============================================================================

#[test]
fn test_compare_to() {
    let source = r#"
        less = 1.compareTo(2)
        equal = 5.compareTo(5)
        greater = 10.compareTo(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List sortBy
// =============================================================================

#[test]
fn test_list_sort_by() {
    let source = r#"
        items = List("banana", "apple", "cherry")
        sorted = items.sortBy((s) -> s.length)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List groupBy
// =============================================================================

#[test]
fn test_list_group_by() {
    let source = r#"
        items = List(1, 2, 3, 4, 5, 6)
        grouped = items.groupBy((x) -> if (x % 2 == 0) "even" else "odd")
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// List partition
// =============================================================================

#[test]
fn test_list_partition() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        result = items.partition((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Map remove
// =============================================================================

#[test]
fn test_map_remove() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        removed = m.remove("b")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List to Map
// =============================================================================

#[test]
fn test_list_to_map() {
    let source = r#"
        items = List("alice", "bob", "carol")
        m = items.toMap((name) -> name, (name) -> name.length)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List to Set
// =============================================================================

#[test]
fn test_list_to_set() {
    let source = r#"
        items = List(1, 2, 2, 3, 3, 3)
        s = items.toSet()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Map entries
// =============================================================================

#[test]
fn test_map_entries() {
    let source = r#"
        m = Map("x", 1, "y", 2)
        entries = m.entries
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Annotations
// =============================================================================

#[test]
fn test_annotation_on_class() {
    let source = r#"
        class Deprecated {
            message: String = ""
        }

        @Deprecated { message = "Use NewConfig" }
        class OldConfig {
            value: Int = 0
        }

        obj = new OldConfig { value = 42 }
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Listing map method
// =============================================================================

#[test]
fn test_listing_map_method() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        doubled = items.map((x) -> x * 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String codePoints property
// =============================================================================

#[test]
fn test_string_codepoints() {
    let source = r#"
        s = "ABC"
        codepoints = s.codePoints
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Duration/DataSize toUnit
// =============================================================================

#[test]
fn test_duration_to_unit() {
    let source = r#"
        d = 2.h
        in_minutes = d.toUnit("min")
        in_seconds = d.toUnit("s")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_datasize_to_unit() {
    let source = r#"
        d = 1.gb
        in_mb = d.toUnit("mb")
        in_kb = d.toUnit("kb")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Mapping toMap conversion
// =============================================================================

#[test]
fn test_mapping_to_map() {
    let source = r#"
        m = new Mapping {
            ["a"] = 1; ["b"] = 2
        }
        converted = m.toMap()
        keys = converted.keys
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: basic/int.pkl - Integer literals and arithmetic
// =============================================================================

#[test]
fn test_upstream_int_literals_comprehensive() {
    let source = r#"
        dec = 123456
        hex = 0xFF
        hex_upper = 0XFF
        bin = 0b10101
        oct = 0o777
        underscore_dec = 1_000_000
        underscore_hex = 0xFF_FF
        underscore_bin = 0b1010_1010
        underscore_oct = 0o77_77
        negative = -42
        zero = 0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_int_arithmetic_comprehensive() {
    let source = r#"
        add = 1 + 2
        sub = 5 - 3
        mul = 4 * 5
        div = 10 ~/ 3
        rem = 10 % 3
        power = 2 ** 10
        power_zero = 5 ** 0
        power_one = 5 ** 1
        neg = -(5)
        combined = 2 + 3 * 4
        combined2 = (2 + 3) * 4
        power_precedence = 2 ** 3 ** 2
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_int_comparison() {
    let source = r#"
        eq = 42 == 42
        neq = 42 != 43
        lt = 1 < 2
        gt = 2 > 1
        lte = 2 <= 2
        gte = 2 >= 2
        lt_false = 2 < 1
        gt_false = 1 > 2
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_int_to_float_promotion() {
    let source = r#"
        add = 1 + 1.5
        sub = 5 - 2.5
        mul = 3 * 1.5
        div = 7 / 2
        compare = 1 < 1.5
        compare2 = 2 > 1.5
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: basic/float.pkl - Float literals and arithmetic
// =============================================================================

#[test]
fn test_upstream_float_literals_comprehensive() {
    let source = r#"
        simple = 1.0
        decimal = 3.14
        exponent = 1e10
        exponent_neg = 1e-10
        exponent_pos = 1e+10
        exponent_decimal = 1.5e2
        negative = -3.14
        zero = 0.0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_arithmetic_comprehensive() {
    let source = r#"
        add = 1.5 + 2.5
        sub = 5.5 - 3.3
        mul = 2.5 * 4.0
        div = 10.0 / 3.0
        power = 2.0 ** 10
        neg = -(3.14)
        combined = 1.5 + 2.5 * 3.0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_comparison() {
    let source = r#"
        eq = 3.14 == 3.14
        neq = 3.14 != 2.71
        lt = 1.0 < 2.0
        gt = 2.0 > 1.0
        lte = 2.0 <= 2.0
        gte = 2.0 >= 2.0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: basic/boolean.pkl - Boolean operations
// =============================================================================

#[test]
fn test_upstream_boolean_comprehensive() {
    let source = r#"
        and1 = true && true
        and2 = true && false
        and3 = false && true
        and4 = false && false
        or1 = true || true
        or2 = true || false
        or3 = false || true
        or4 = false || false
        not1 = !true
        not2 = !false
        eq1 = true == true
        eq2 = true == false
        neq1 = true != false
        neq2 = true != true
        xor1 = true.xor(true)
        xor2 = true.xor(false)
        xor3 = false.xor(true)
        xor4 = false.xor(false)
        implies1 = true.implies(true)
        implies2 = true.implies(false)
        implies3 = false.implies(true)
        implies4 = false.implies(false)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: basic/null.pkl - Null handling
// =============================================================================

#[test]
fn test_upstream_null_equality() {
    let source = r#"
        null_eq_null = null == null
        null_neq_one = null != 1
        one_neq_null = 1 != null
        null_eq_one = null == 1
        one_eq_null = 1 == null
        null_neq_null = null != null
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_null_coalescing_comprehensive() {
    let source = r#"
        r1 = null ?? 2
        r2 = 1 ?? 2
        r3 = 1 ?? 2 ?? 3
        r4 = null ?? null ?? 3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_null_safe_access() {
    let source = r#"
        r1 = null?.toUpperCase()
        r2 = "Pigeon"?.toUpperCase()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_null_member_access_error() {
    let source = r#"
        r1 = null.foo
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Upstream: basic/let.pkl - Let bindings
// =============================================================================

#[test]
fn test_upstream_let_basic() {
    let source = r#"
        res1 = let (x = 42) x + 1
        res2 = let (res2 = 42) res2 + 1
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_nested() {
    let source = r#"
        res1 = let (x = 1) let (y = 2) x + y + x
        res2 = let (x = 1) let (x = 2) x + x
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_with_object() {
    let source = r#"
        res = let (price = 500) new {
            lowestPrice = price - 100
            highestPrice = price + 100
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_with_method_call() {
    let source = r#"
        res = let (str = "Pigeon".reverse()) str.reverse()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_in_function() {
    let source = r#"
        function f(a) = let (b = a * 2) a + b
        res = f(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_in_lambda() {
    let source = r#"
        local g = (a) -> let (b = a * 2) a + b
        res = g.apply(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_chain() {
    let source = r#"
        res = let (x = 1) let (y = x) let (z = y) x + y + z
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_in_class() {
    let source = r#"
        class Lets {
            x = 2
            y = let (z = 2) z + x
            function f(x) = let (z = 2) z + x
            z = f(x)
        }
        res = new Lets { x = 4 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_let_with_intseq_map() {
    let source = r#"
        res = IntSeq(1, 5).map((n) -> let (x = n + 1) x + 1)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: basic/nonNull.pkl - Non-null assertion
// =============================================================================

#[test]
fn test_upstream_non_null_types() {
    let source = r#"
        r1 = 123!!
        r2 = let (x = 123) x!!
        r3 = 1.23.ms!!
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_non_null_on_null_error() {
    let source = r#"
        r1 = null!!
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Upstream: basic/if.pkl - If expressions
// =============================================================================

#[test]
fn test_upstream_if_basic() {
    let source = r#"
        r1 = if (true) "yes" else "no"
        r2 = if (false) "yes" else "no"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_if_nested() {
    let source = r#"
        r1 = if (true) if (true) 1 else 2 else 3
        r2 = if (true) if (false) 1 else 2 else 3
        r3 = if (false) if (true) 1 else 2 else 3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_if_with_expressions() {
    let source = r#"
        x = 10
        r1 = if (x > 5) "big" else "small"
        r2 = if (x > 20) "big" else "small"
        r3 = if (x == 10) "ten" else "other"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_if_in_string_interpolation() {
    // Note: nested quotes in interpolation need special handling
    let source = r#"
        x = true
        local yes = "yes"
        local no = "no"
        res = "The answer is \(if (x) yes else no)"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: basic/string.pkl - String interpolation comprehensive
// =============================================================================

#[test]
fn test_upstream_string_interpolation_types() {
    let source = r#"
        r_int = "\(42)"
        r_float = "\(1.23)"
        r_string = "\("Pigeon")"
        r_bool = "\(false)"
        r_duration = "\(1.23.ns)"
        r_datasize = "\(1.23.mb)"
        r_pair = "\(Pair(1, 2))"
        r_list = "\(List(1, 2, 3))"
        r_set = "\(Set(1, 2, 3))"
        r_map = "\(Map(1, 2, 3, 4))"
        r_null = "\(null)"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_nested_interpolation() {
    let source = r#"
        local str1 = "How"
        local str2 = "you"
        res1 = "\(str1) are \(str2) today? Are \(str2) hungry?"
        res2 = "Is \(str1 + str2) a word?"
        res3 = "Is \("soma") a word?"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_dollar_sign() {
    let source = r#"
        r1 = "123$"
        r2 = "$123"
        r3 = "$&%"
        r4 = "$"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_subscript() {
    let source = r#"
        s = "this"
        r = s[2]
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_subscript_out_of_bounds() {
    let source = r#"
        s = "this"
        r = s[10]
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Upstream: basic/multiLineString.pkl - Multi-line string tests
// =============================================================================

#[test]
fn test_upstream_multiline_trimming_leading_trailing() {
    let source = r##"
        r1 = """
            leading and trailing newline trimmed
            """
    "##;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_multiline_whitespace_trimming() {
    let source = r##"
        r1 = """
            leading
            whitespace
            trimmed
            """
        r2 = """
            leading
              whitespace
                partially
                  trimmed
            """
    "##;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_multiline_quotes() {
    let source = r##"
        r1 = """
            "AS IS"
            """
        r2 = """
            ""AS IS""
            """
    "##;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/int.pkl - Int API methods
// =============================================================================

#[test]
fn test_upstream_int_api_properties() {
    let source = r#"
        sign_pos = 42.sign
        sign_neg = (-42).sign
        sign_zero = 0.sign
        abs_pos = 42.abs
        abs_neg = (-42).abs
        is_positive = 42.isPositive
        is_negative = (-42).isPositive
        is_non_zero = 42.isNonZero
        is_zero = 0.isNonZero
        is_even_4 = 4.isEven
        is_even_5 = 5.isEven
        is_odd_5 = 5.isOdd
        is_odd_4 = 4.isOdd
        inv = 0.inv
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_int_api_bitwise() {
    let source = r#"
        shl = 1.shl(4)
        shr = 16.shr(2)
        ushr = (-1).ushr(60)
        and = 0xFF.and(0x0F)
        or = 0xF0.or(0x0F)
        xor = 0xFF.xor(0x0F)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_int_api_conversions() {
    let source = r#"
        to_float = 42.toFloat()
        to_string = 42.toString()
        to_radix_2 = 255.toRadixString(2)
        to_radix_8 = 255.toRadixString(8)
        to_radix_16 = 255.toRadixString(16)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_int_is_between() {
    let source = r#"
        r1 = 5.isBetween(1, 10)
        r2 = 1.isBetween(1, 10)
        r3 = 10.isBetween(1, 10)
        r4 = 0.isBetween(1, 10)
        r5 = 11.isBetween(1, 10)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/float.pkl - Float API methods
// =============================================================================

#[test]
fn test_upstream_float_api_properties() {
    let source = r#"
        sign_pos = 3.14.sign
        sign_neg = (-3.14).sign
        sign_zero = 0.0.sign
        abs_pos = 3.14.abs
        abs_neg = (-3.14).abs
        is_positive = 3.14.isPositive
        is_negative = (-3.14).isPositive
        is_non_zero = 3.14.isNonZero
        is_zero = 0.0.isNonZero
        is_finite = 3.14.isFinite
        is_integer_yes = 3.0.isInteger
        is_integer_no = 3.14.isInteger
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_api_rounding() {
    let source = r#"
        ceil_pos = 3.2.ceil
        ceil_neg = (-3.2).ceil
        floor_pos = 3.8.floor
        floor_neg = (-3.8).floor
        round_up = 3.5.round
        round_down = 3.4.round
        truncate_pos = 3.7.truncate
        truncate_neg = (-3.7).truncate
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_api_conversions() {
    let source = r#"
        to_int = 3.14.toInt()
        to_string = 3.14.toString()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_is_between() {
    let source = r#"
        r1 = 5.5.isBetween(1.0, 10.0)
        r2 = 1.0.isBetween(1.0, 10.0)
        r3 = 10.0.isBetween(1.0, 10.0)
        r4 = 0.5.isBetween(1.0, 10.0)
        r5 = 10.5.isBetween(1.0, 10.0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/string.pkl - String API methods comprehensive
// =============================================================================

#[test]
fn test_upstream_string_api_properties() {
    let source = r#"
        empty = "".isEmpty
        notempty = "abc".isEmpty
        length = "hello".length
        length_empty = "".length
        last_index = "hello".lastIndex
        chars = "abc".chars
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_search() {
    let source = r#"
        contains_yes = "hello world".contains("world")
        contains_no = "hello world".contains("xyz")
        starts_with = "hello".startsWith("hel")
        starts_with_no = "hello".startsWith("xyz")
        ends_with = "hello".endsWith("llo")
        ends_with_no = "hello".endsWith("xyz")
        index_of = "hello world".indexOf("world")
        index_of_none = "hello".indexOf("xyz")
        last_index_of = "abcabc".lastIndexOf("abc")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_transform() {
    let source = r#"
        upper = "hello".toUpperCase()
        lower = "HELLO".toLowerCase()
        reverse = "hello".reverse()
        trim = "  hello  ".trim()
        trim_start = "  hello  ".trimStart()
        trim_end = "  hello  ".trimEnd()
        replace_first = "hello hello".replaceFirst("hello", "world")
        replace_all = "hello hello".replaceAll("hello", "world")
        replace_range = "hello".replaceRange(1, 3, "XYZ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_take_drop() {
    let source = r#"
        take = "hello".take(3)
        take_zero = "hello".take(0)
        drop = "hello".drop(2)
        drop_all = "hello".drop(5)
        take_last = "hello".takeLast(3)
        drop_last = "hello".dropLast(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_take_drop_while() {
    let source = r#"
        take_while = "aabccc".takeWhile((c) -> c == "a")
        drop_while = "aabccc".dropWhile((c) -> c == "a")
        take_last_while = "aabccc".takeLastWhile((c) -> c == "c")
        drop_last_while = "aabccc".dropLastWhile((c) -> c == "c")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_split() {
    let source = r#"
        split = "a,b,c".split(",")
        split_limit = "a,b,c,d".split(",")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_pad() {
    let source = r#"
        pad_start = "42".padStart(5, "0")
        pad_end = "42".padEnd(5, "0")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_conversions() {
    let source = r#"
        to_int = "42".toInt()
        to_float = "3.14".toFloat()
        to_boolean_true = "true".toBoolean()
        to_boolean_false = "false".toBoolean()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_repeat() {
    let source = r#"
        r1 = "abc".repeat(3)
        r2 = "x".repeat(0)
        r3 = "ha".repeat(1)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_api_substring() {
    let source = r#"
        r1 = "hello world".substring(0, 5)
        r2 = "hello world".substring(6, 11)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/list.pkl - List API methods comprehensive
// =============================================================================

#[test]
fn test_upstream_list_api_properties() {
    let source = r#"
        empty = List().isEmpty
        notempty = List(1, 2, 3).isEmpty
        length = List(1, 2, 3).length
        first = List(1, 2, 3).first
        rest = List(1, 2, 3).rest
        last = List(1, 2, 3).last
        single = List(42).single
        last_index = List(1, 2, 3).lastIndex
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_filter_map() {
    let source = r#"
        filtered = List(1, 2, 3, 4, 5).filter((x) -> x > 3)
        mapped = List(1, 2, 3).map((x) -> x * 10)
        flat_mapped = List(1, 2, 3).flatMap((x) -> List(x, x * 10))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_fold_reduce() {
    let source = r#"
        folded = List(1, 2, 3, 4).fold(0, (acc, x) -> acc + x)
        folded_str = List("a", "b", "c").fold("", (acc, x) -> acc + x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_any_every() {
    let source = r#"
        any_true = List(1, 2, 3).any((x) -> x > 2)
        any_false = List(1, 2, 3).any((x) -> x > 5)
        every_true = List(1, 2, 3).every((x) -> x > 0)
        every_false = List(1, 2, 3).every((x) -> x > 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_find() {
    let source = r#"
        found = List(1, 2, 3, 4).find((x) -> x > 2)
        find_index = List(1, 2, 3, 4).findIndex((x) -> x > 2)
        find_last = List(1, 2, 3, 4).findLast((x) -> x > 2)
        find_last_index = List(1, 2, 3, 4).findLastIndex((x) -> x > 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_count() {
    let source = r#"
        r1 = List(1, 2, 3, 4, 5).count((x) -> x > 3)
        r2 = List(1, 2, 3, 4, 5).count((x) -> x > 10)
        r3 = List(1, 2, 3, 4, 5).count((x) -> x > 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_group_by() {
    let source = r#"
        grouped = List(1, 2, 3, 4, 5, 6).groupBy((x) -> if (x % 2 == 0) "even" else "odd")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_sort_comprehensive() {
    let source = r#"
        sorted = List(3, 1, 4, 1, 5).sort()
        sorted_by = List("banana", "apple", "cherry").sortBy((x) -> x.length)
        reversed = List(1, 2, 3).reverse()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_distinct_flatten() {
    let source = r#"
        distinct = List(1, 2, 2, 3, 3, 3).distinct
        flattened = List(List(1, 2), List(3, 4), List(5)).flatten
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_take_drop_while() {
    let source = r#"
        take_while = List(1, 2, 3, 4, 5).takeWhile((x) -> x < 4)
        drop_while = List(1, 2, 3, 4, 5).dropWhile((x) -> x < 4)
        take_last_while = List(1, 2, 3, 4, 5).takeLastWhile((x) -> x > 3)
        drop_last_while = List(1, 2, 3, 4, 5).dropLastWhile((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_zip_comprehensive() {
    let source = r#"
        zipped = List(1, 2, 3).zip(List("a", "b", "c"))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_partition() {
    let source = r#"
        partitioned = List(1, 2, 3, 4, 5).partition((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_conversions() {
    let source = r#"
        to_set = List(1, 2, 2, 3).toSet()
        to_list = List(1, 2, 3).toList()
        join = List("a", "b", "c").join(", ")
        join_empty = List("a", "b", "c").join("")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_map_indexed() {
    let source = r#"
        result = List("a", "b", "c").mapIndexed((i, x) -> "\(i):\(x)")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_fold_indexed() {
    let source = r#"
        result = List(10, 20, 30).foldIndexed(0, (i, acc, x) -> acc + i * x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_list_api_min_max_by() {
    let source = r#"
        min_by = List("banana", "apple", "kiwi").minBy((x) -> x.length)
        max_by = List("banana", "apple", "kiwi").maxBy((x) -> x.length)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/duration.pkl - Duration API
// =============================================================================

#[test]
fn test_upstream_duration_construction() {
    let source = r#"
        ns = 1.ns
        us = 1.us
        ms = 1.ms
        s = 1.s
        min = 1.min
        h = 1.h
        d = 1.d
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_duration_properties() {
    let source = r#"
        d = 90.min
        value = d.value
        unit = d.unit
        is_positive = d.isPositive
        is_neg = (-(5.s)).isPositive
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_duration_arithmetic_comprehensive() {
    let source = r#"
        add = 1.s + 500.ms
        sub = 2.h - 30.min
        mul = 2.5.s * 3
        div = 10.s / 2
        negate = -(5.s)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_duration_comparison() {
    let source = r#"
        eq = 60.s == 1.min
        lt = 1.s < 1.min
        gt = 1.h > 1.min
        lte = 60.s <= 1.min
        gte = 1.min >= 60.s
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_duration_to_unit_comprehensive() {
    let source = r#"
        r1 = 1.h.toUnit("min")
        r2 = 1.h.toUnit("s")
        r3 = 1.d.toUnit("h")
        r4 = 1.min.toUnit("ms")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/dataSize.pkl - DataSize API
// =============================================================================

#[test]
fn test_upstream_datasize_construction() {
    let source = r#"
        b = 1.b
        kb = 1.kb
        mb = 1.mb
        gb = 1.gb
        tb = 1.tb
        pb = 1.pb
        kib = 1.kib
        mib = 1.mib
        gib = 1.gib
        tib = 1.tib
        pib = 1.pib
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_datasize_properties() {
    let source = r#"
        d = 500.mb
        value = d.value
        unit = d.unit
        is_positive = d.isPositive
        is_neg = (-(5.kb)).isPositive
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_datasize_arithmetic_comprehensive() {
    let source = r#"
        add = 1.gb + 500.mb
        sub = 2.gb - 500.mb
        mul = 2.5.mb * 4
        div = 10.gb / 2
        negate = -(5.mb)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_datasize_comparison() {
    let source = r#"
        eq = 1024.kb == 1.mb
        lt = 1.kb < 1.mb
        gt = 1.gb > 1.mb
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_datasize_to_unit_comprehensive() {
    let source = r#"
        r1 = 1.gb.toUnit("mb")
        r2 = 1.gb.toUnit("kb")
        r3 = 1.mb.toUnit("b")
        r4 = 1.gib.toUnit("mib")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/map.pkl - Map API
// =============================================================================

#[test]
fn test_upstream_map_construction() {
    let source = r#"
        empty = Map()
        single = Map("a", 1)
        multi = Map("a", 1, "b", 2, "c", 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_map_api_properties() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        empty = m.isEmpty
        length = m.length
        keys = m.keys
        values = m.values
        entries = m.entries
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_map_api_access() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        contains_key = m.containsKey("b")
        contains_key_no = m.containsKey("z")
        get = m["b"]
        get_or_null = m.getOrNull("z")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_map_api_transform() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        filtered = m.filter((k, v) -> v > 1)
        mapped = m.map((k, v) -> Pair(k, v * 10))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_map_api_put_remove() {
    let source = r#"
        m = Map("a", 1, "b", 2)
        with_new = m.put("c", 3)
        with_replaced = m.put("a", 99)
        removed = m.remove("a")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_map_api_fold() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        sum = m.fold(0, (acc, k, v) -> acc + v)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_map_api_every_any() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        any_gt2 = m.any((k, v) -> v > 2)
        any_gt5 = m.any((k, v) -> v > 5)
        every_gt0 = m.every((k, v) -> v > 0)
        every_gt2 = m.every((k, v) -> v > 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/set.pkl - Set API
// =============================================================================

#[test]
fn test_upstream_set_construction() {
    let source = r#"
        empty = Set()
        single = Set(1)
        multi = Set(1, 2, 3)
        deduped = Set(1, 2, 2, 3, 3, 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_properties() {
    let source = r#"
        s = Set(1, 2, 3)
        empty = s.isEmpty
        length = s.length
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_operations() {
    let source = r#"
        s1 = Set(1, 2, 3)
        s2 = Set(2, 3, 4)
        contains = s1.contains(2)
        no_contains = s1.contains(5)
        add = s1.add(4)
        add_existing = s1.add(2)
        remove = s1.remove(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_set_operations() {
    let source = r#"
        s1 = Set(1, 2, 3)
        s2 = Set(2, 3, 4)
        intersection = s1.intersect(s2)
        difference = s1.difference(s2)
        union = s1.union(s2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_transform() {
    let source = r#"
        s = Set(1, 2, 3, 4, 5)
        filtered = s.filter((x) -> x > 3)
        mapped = s.map((x) -> x * 10)
        flat_mapped = s.flatMap((x) -> Set(x, x * 10))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_fold() {
    let source = r#"
        s = Set(1, 2, 3)
        sum = s.fold(0, (acc, x) -> acc + x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_any_every() {
    let source = r#"
        s = Set(1, 2, 3, 4, 5)
        any_gt3 = s.any((x) -> x > 3)
        any_gt10 = s.any((x) -> x > 10)
        every_gt0 = s.every((x) -> x > 0)
        every_gt3 = s.every((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_set_api_conversions() {
    let source = r#"
        s = Set(3, 1, 2)
        to_list = s.toList()
        to_set = s.toSet()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/pair.pkl - Pair API
// =============================================================================

#[test]
fn test_upstream_pair_api() {
    let source = r#"
        p = Pair("hello", 42)
        first = p.first
        second = p.second
        key = p.key
        value = p.value
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/intSeq.pkl - IntSeq API
// =============================================================================

#[test]
fn test_upstream_intseq_api() {
    let source = r#"
        seq = IntSeq(1, 5)
        start = seq.start
        end = seq.end
        step = seq.step
        to_list = seq.toList()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_intseq_step() {
    let source = r#"
        seq = IntSeq(0, 10).step(2)
        to_list = seq.toList()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_intseq_map() {
    let source = r#"
        result = IntSeq(1, 5).map((n) -> n * n)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: api/regex.pkl - Regex API
// =============================================================================

#[test]
fn test_upstream_regex_construction() {
    let source = r#"
        r = Regex("[a-z]+")
        pattern = r.pattern
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_regex_match_methods() {
    let source = r#"
        r = Regex("[0-9]+")
        matches_full = r.matchEntire("12345")
        find_in = "abc 123 def 456".matches(Regex("[0-9]+"))
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_upstream_string_matches_regex() {
    let source = r#"
        r1 = "hello123".matches(Regex("[a-z]+[0-9]+"))
        r2 = "hello".matches(Regex("[0-9]+"))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Listing/Mapping comprehensive
// =============================================================================

#[test]
fn test_upstream_listing_comprehensive() {
    let source = r#"
        l = new Listing { 1; 2; 3; 4; 5 }
        length = l.length
        isEmpty = l.isEmpty
        first = l.first
        last = l.last
        rest = l.rest
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_listing_filter_map() {
    let source = r#"
        l = new Listing { 1; 2; 3; 4; 5 }
        filtered = l.filter((x) -> x > 3)
        mapped = l.map((x) -> x * 10)
        to_list = l.toList()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_listing_fold() {
    let source = r#"
        l = new Listing { 1; 2; 3; 4; 5 }
        sum = l.fold(0, (acc, x) -> acc + x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_mapping_comprehensive() {
    let source = r#"
        m = new Mapping {
            ["a"] = 1
            ["b"] = 2
            ["c"] = 3
        }
        length = m.length
        isEmpty = m.isEmpty
        keys = m.keys
        values = m.values
        contains_key = m.containsKey("b")
        no_contains = m.containsKey("z")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_mapping_filter_map() {
    let source = r#"
        m = new Mapping {
            ["a"] = 1
            ["b"] = 2
            ["c"] = 3
        }
        filtered = m.filter((k, v) -> v > 1)
        mapped = m.map((k, v) -> Pair(k, v * 10))
        to_map = m.toMap()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Object features
// =============================================================================

#[test]
fn test_upstream_dynamic_object() {
    let source = r#"
        d = new {
            name = "Pigeon"
            age = 42
            nested = new {
                x = 1
                y = 2
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_object_spread_comprehensive() {
    let source = r#"
        base = new Listing { 1; 2; 3 }
        extended = new Listing { ...base; 4; 5 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_for_generator_comprehensive() {
    let source = r#"
        squares = new Listing {
            for (i in IntSeq(1, 5)) {
                i * i
            }
        }
        filtered = new Listing {
            for (i in IntSeq(1, 10)) {
                when (i % 2 == 0) {
                    i
                }
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_for_generator_mapping() {
    let source = r#"
        names = List("alice", "bob", "charlie")
        result = new Mapping {
            for (name in names) {
                [name] = name.length
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_when_generator_comprehensive() {
    let source = r#"
        debug = true
        config = new {
            name = "app"
            when (debug) {
                logLevel = "DEBUG"
                verbose = true
            }
        }
        config2 = new {
            name = "app"
            when (!debug) {
                logLevel = "DEBUG"
                verbose = true
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Class features
// =============================================================================

#[test]
fn test_upstream_class_with_methods_comprehensive() {
    let source = r#"
        class Person {
            name: String
            age: Int

            function greet() = "Hello, I'm \(name)"
            function isAdult() = age >= 18
        }

        p = new Person {
            name = "Pigeon"
            age = 42
        }
        greeting = p.greet()
        adult = p.isAdult()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_class_inheritance_comprehensive() {
    let source = r#"
        open class Animal {
            name: String
            sound: String = "..."
            function speak() = "\(name) says \(sound)"
        }

        class Dog extends Animal {
            sound = "Woof"
            breed: String
        }

        class Cat extends Animal {
            sound = "Meow"
            indoor: Boolean = true
        }

        dog = new Dog { name = "Rex"; breed = "German Shepherd" }
        cat = new Cat { name = "Whiskers" }
        dog_speaks = dog.speak()
        cat_speaks = cat.speak()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_class_abstract_comprehensive() {
    let source = r#"
        abstract class Shape {
            function area(): Float
        }

        class Circle extends Shape {
            radius: Float
            function area() = 3.14159 * radius * radius
        }

        class Rectangle extends Shape {
            width: Float
            height: Float
            function area() = width * height
        }

        circle = new Circle { radius = 5.0 }
        rect = new Rectangle { width = 3.0; height = 4.0 }
        circle_area = circle.area()
        rect_area = rect.area()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Type checking and casting
// =============================================================================

#[test]
fn test_upstream_is_type_comprehensive() {
    let source = r#"
        r_int = 42 is Int
        r_float = 3.14 is Float
        r_string = "hello" is String
        r_bool = true is Boolean
        r_list = List(1, 2) is List
        r_set = Set(1, 2) is Set
        r_map = Map("a", 1) is Map
        r_null = null is Null
        r_int_not_string = 42 is String
        r_string_not_int = "hello" is Int
        r_number_int = 42 is Number
        r_number_float = 3.14 is Number
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_as_type_comprehensive() {
    let source = r#"
        r_int = (42 as Int)
        r_string = ("hello" as String)
        r_number = (42 as Number)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_as_type_failure() {
    let source = r#"
        r = (42 as String)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Upstream: Module-level functions
// =============================================================================

#[test]
fn test_upstream_module_function_comprehensive() {
    let source = r#"
        function factorial(n) = if (n <= 1) 1 else n * factorial(n - 1)
        function fibonacci(n) = if (n <= 1) n else fibonacci(n - 1) + fibonacci(n - 2)

        fact5 = factorial(5)
        fact10 = factorial(10)
        fib10 = fibonacci(10)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_function_as_value() {
    let source = r#"
        local double = (x) -> x * 2
        local add = (a, b) -> a + b

        r1 = double.apply(21)
        r2 = add.apply(20, 22)
        r3 = List(1, 2, 3).map(double)
        r4 = List(1, 2, 3).map((x) -> x * x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Null propagation and optional chaining
// =============================================================================

#[test]
fn test_upstream_optional_chaining_comprehensive() {
    let source = r#"
        r1 = null?.length
        r2 = "hello"?.length
        r3 = null?.toUpperCase()
        r4 = "hello"?.toUpperCase()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Throw expressions
// =============================================================================

#[test]
fn test_upstream_throw_in_conditional() {
    let source = r#"
        function validate(x) = if (x > 0) x else throw("must be positive")
        r1 = validate(42)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_throw_error() {
    let source = r#"
        r = throw("something went wrong")
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Upstream: String escape sequences comprehensive
// =============================================================================

#[test]
fn test_upstream_string_escapes_comprehensive() {
    let source = r#"
        tab = "a\tb"
        newline = "a\nb"
        cr = "a\rb"
        backslash = "a\\b"
        quote = "a\"b"
        unicode_basic = "\u{41}"
        unicode_emoji = "\u{1F600}"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Amends/Extends comprehensive
// =============================================================================

#[test]
fn test_upstream_amends_comprehensive() {
    let source = r#"
        class Config {
            host: String = "localhost"
            port: Int = 8080
            debug: Boolean = false
        }

        base = new Config {}
        dev = (base) { debug = true; port = 3000 }
        prod = (base) { host = "prod.example.com" }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_amends_nested() {
    let source = r#"
        class Database {
            host: String = "localhost"
            port: Int = 5432
        }
        class AppConfig {
            name: String
            db: Database
        }

        base = new AppConfig {
            name = "myapp"
            db = new Database {}
        }
        modified = (base) {
            db { port = 5433 }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Complex real-world patterns
// =============================================================================

#[test]
fn test_upstream_realworld_server_config() {
    let source = r#"
        class Server {
            host: String
            port: Int
            ssl: Boolean = false
        }

        class Cluster {
            name: String
            servers: Listing<Server>
        }

        cluster = new Cluster {
            name = "production"
            servers {
                new { host = "web1.example.com"; port = 443; ssl = true }
                new { host = "web2.example.com"; port = 443; ssl = true }
                new { host = "web3.example.com"; port = 443; ssl = true }
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_realworld_feature_flags() {
    let source = r#"
        class FeatureFlag {
            enabled: Boolean = false
            rollout: Float = 0.0
            description: String
        }

        features = new Mapping {
            ["dark-mode"] = new FeatureFlag {
                enabled = true
                rollout = 1.0
                description = "Dark mode UI theme"
            }
            ["new-checkout"] = new FeatureFlag {
                rollout = 0.5
                description = "New checkout flow"
            }
        }

        enabled_features = features.filter((k, v) -> v.enabled).keys
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_realworld_pipeline() {
    let source = r#"
        local data = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

        result = data
            .filter((x) -> x % 2 == 0)
            .map((x) -> x * x)
            .fold(0, (acc, x) -> acc + x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Edge cases and corner cases
// =============================================================================

#[test]
fn test_upstream_empty_collections() {
    let source = r#"
        empty_list = List()
        empty_set = Set()
        empty_map = Map()
        empty_listing = new Listing {}
        empty_mapping = new Mapping {}
        list_empty = List().isEmpty
        set_empty = Set().isEmpty
        map_empty = Map().isEmpty
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_single_element_collections() {
    let source = r#"
        list1 = List(42)
        set1 = Set(42)
        map1 = Map("key", "value")
        list_first = List(42).first
        list_last = List(42).last
        list_single = List(42).single
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_nested_collections() {
    let source = r#"
        nested_list = List(List(1, 2), List(3, 4))
        nested_map = Map("a", Map("x", 1), "b", Map("y", 2))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_edge_cases() {
    let source = r#"
        empty = ""
        empty_length = "".length
        empty_is_empty = "".isEmpty
        single_char = "a"
        unicode = "Hello 🌍"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_number_edge_cases() {
    let source = r#"
        max_safe_int = 9007199254740991
        neg_max = -9007199254740991
        zero_int = 0
        neg_zero_float = -0.0
        very_small = 0.000000001
        very_large = 999999999999.0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Property access patterns
// =============================================================================

#[test]
fn test_upstream_chained_access() {
    let source = r#"
        class Inner { value: Int }
        class Outer { inner: Inner }

        obj = new Outer { inner = new Inner { value = 42 } }
        result = obj.inner.value
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_method_chaining() {
    let source = r#"
        result = "  Hello, World!  "
            .trim()
            .toLowerCase()
            .replaceAll("world", "pkl")
            .toUpperCase()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Generators with complex conditions
// =============================================================================

#[test]
fn test_upstream_nested_for_generators() {
    let source = r#"
        matrix = new Listing {
            for (i in IntSeq(1, 3)) {
                for (j in IntSeq(1, 3)) {
                    i * 10 + j
                }
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_for_with_index_and_condition() {
    let source = r#"
        items = List("apple", "banana", "cherry", "date")
        result = new Listing {
            for (_idx, item in items) {
                when (item.length > 5) {
                    item.toUpperCase()
                }
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Listing/Mapping amendment
// =============================================================================

#[test]
fn test_upstream_listing_amendment() {
    let source = r#"
        base = new Listing { 1; 2; 3 }
        amended = (base) { 4; 5 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_mapping_amendment() {
    let source = r#"
        base = new Mapping { ["a"] = 1; ["b"] = 2 }
        amended = (base) { ["c"] = 3; ["a"] = 10 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Computed properties with complex expressions
// =============================================================================

#[test]
fn test_upstream_computed_property_expressions() {
    let source = r#"
        local names = List("alice", "bob")
        result = new Mapping {
            for (name in names) {
                [name.toUpperCase()] = name.length
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Multiple return types from functions
// =============================================================================

#[test]
fn test_upstream_function_returning_different_types() {
    let source = r#"
        function describe(x) = if (x is Int) "integer" else if (x is String) "string" else "other"
        r1 = describe(42)
        r2 = describe("hello")
        r3 = describe(3.14)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: String regex methods
// =============================================================================

#[test]
fn test_upstream_string_regex_methods() {
    let source = r#"
        r1 = "hello123world456".replaceAll(Regex("[0-9]+"), "NUM")
        r2 = "hello world".matches(Regex("hello.*"))
        r3 = "abc123def".replaceFirst(Regex("[0-9]+"), "X")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Math methods on Float
// =============================================================================

#[test]
fn test_upstream_float_math_comprehensive() {
    let source = r#"
        sqrt = 16.0.sqrt
        cbrt = 27.0.cbrt
        exp = 1.0.exp
        log = 2.718281828.log
        log2 = 8.0.log2
        log10 = 1000.0.log10
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_trig() {
    let source = r#"
        sin = 0.0.sin
        cos = 0.0.cos
        tan = 0.0.tan
        asin = 1.0.asin
        acos = 1.0.acos
        atan = 1.0.atan
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Pipe operator
// =============================================================================

#[test]
fn test_upstream_pipe_comprehensive() {
    let source = r#"
        r1 = 42 |> (x) -> x * 2
        r2 = "hello" |> (s) -> s.toUpperCase()
        r3 = List(3, 1, 2) |> (l) -> l.sort()
        r4 = 5 |> (x) -> x * 2 |> (x) -> x + 1
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: List toMap
// =============================================================================

#[test]
fn test_upstream_list_to_map_comprehensive() {
    let source = r#"
        result = List(Pair("a", 1), Pair("b", 2), Pair("c", 3)).toMap()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Int/Float min/max
// =============================================================================

#[test]
fn test_upstream_int_min_max() {
    let source = r#"
        r1 = 5.min(3)
        r2 = 5.max(3)
        r3 = (-5).min(3)
        r4 = (-5).max(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_float_min_max() {
    let source = r#"
        r1 = 5.5.min(3.3)
        r2 = 5.5.max(3.3)
        r3 = (-5.5).min(3.3)
        r4 = (-5.5).max(3.3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: String sha/base64 methods
// =============================================================================

#[test]
fn test_upstream_string_hash_methods() {
    let source = r#"
        sha1 = "hello".sha1
        sha256 = "hello".sha256
        md5 = "hello".md5
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_upstream_string_base64() {
    let source = r#"
        encoded = "hello".base64
        decoded = "aGVsbG8=".base64Decoded
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Listing/Mapping with typed elements
// =============================================================================

#[test]
fn test_upstream_typed_listing() {
    let source = r#"
        class Item {
            name: String
            price: Float
        }

        items = new Listing {
            new Item { name = "Apple"; price = 1.50 }
            new Item { name = "Banana"; price = 0.75 }
            new Item { name = "Cherry"; price = 3.00 }
        }

        total = items.toList().fold(0.0, (acc, item) -> acc + item.price)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Mapping with iteration
// =============================================================================

#[test]
fn test_upstream_mapping_iteration() {
    let source = r#"
        m = new Mapping {
            ["x"] = 10
            ["y"] = 20
            ["z"] = 30
        }
        doubled = m.map((k, v) -> Pair(k, v * 2))
        sum_values = m.fold(0, (acc, k, v) -> acc + v)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Object with hidden and local
// =============================================================================

#[test]
fn test_upstream_hidden_local_comprehensive() {
    let source = r#"
        class Config {
            local multiplier = 10
            hidden base_value: Int
            computed_value: Int = base_value * multiplier
        }

        config = new Config {
            base_value = 5
        }
        result = config.computed_value
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Typealias comprehensive
// =============================================================================

#[test]
fn test_upstream_typealias_comprehensive() {
    let source = r#"
        typealias Name = String
        typealias Age = Int
        typealias StringOrInt = String|Int

        name: Name = "Pigeon"
        age: Age = 42
        value: StringOrInt = "hello"
        value2: StringOrInt = 42
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Complex amendment chains
// =============================================================================

#[test]
fn test_upstream_amendment_chain() {
    let source = r#"
        class Config {
            host: String = "localhost"
            port: Int = 8080
            workers: Int = 1
        }

        base = new Config {}
        step1 = (base) { port = 3000 }
        step2 = (step1) { workers = 4 }
        step3 = (step2) { host = "0.0.0.0" }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: List methods - add, addAll, replace
// =============================================================================

#[test]
fn test_upstream_list_add_operations() {
    let source = r#"
        base = List(1, 2, 3)
        added = base.add(4)
        added_all = base.addAll(List(4, 5, 6))
        replaced = base.replaceRange(1, 2, List(20, 30))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Upstream: Map merge operations
// =============================================================================

#[test]
fn test_upstream_map_merge() {
    let source = r#"
        m1 = Map("a", 1, "b", 2)
        m2 = Map("b", 20, "c", 3)
        merged = Map("a", 1, "b", 20, "c", 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}
