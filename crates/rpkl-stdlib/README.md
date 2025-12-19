# rpkl-stdlib

Standard library implementations for PKL.

## Included Methods

### String
- `length`, `isEmpty`, `isBlank`
- `contains`, `startsWith`, `endsWith`
- `toUpperCase`, `toLowerCase`, `capitalize`
- `trim`, `trimStart`, `trimEnd`
- `split`, `chars`, `getOrNull`
- `replaceAll`, `replaceFirst`, `replaceRange`
- `take`, `drop`, `substring`
- `padStart`, `padEnd`, `repeat`
- `indexOf`, `lastIndexOf`

### Int / Float
- `abs`, `sign`, `ceil`, `floor`, `round`
- `isEven`, `isOdd`, `isPositive`, `isNegative`
- `sqrt`, `pow`, `exp`, `log`, `log10`, `log2`
- `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
- `toInt`, `toFloat`, `toString`
- `isBetween`, `compareTo`

### List
- `length`, `isEmpty`, `first`, `last`
- `map`, `filter`, `fold`, `flatMap`
- `take`, `drop`, `takeLast`, `dropLast`
- `any`, `every`, `contains`, `indexOf`
- `reverse`, `distinct`, `flatten`, `join`

### Listing / Mapping
- `toList`, `toMap`
- `length`, `isEmpty`
- `join`, `map` (Listing)

### Map
- `entries`, `keys`, `values`
- `length`, `isEmpty`

### Boolean
- `xor`, `implies`

### Pair
- `first`, `second`, `key`, `value`

## Usage

```rust
use rpkl_runtime::ExternalRegistry;
use rpkl_stdlib::stdlib_registry;

// Get a registry with all stdlib functions
let registry: ExternalRegistry = stdlib_registry();
```

## License

Apache-2.0
