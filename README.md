# purescript-temporal

PureScript bindings to the [TC39 Temporal API](https://tc39.es/proposal-temporal/docs/) for modern date/time handling.

Temporal replaces the legacy `Date` object with a comprehensive set of types that properly model time zones, calendar-aware arithmetic, and nanosecond precision. This library exposes the full Temporal API through idiomatic PureScript — fallible operations return `Maybe`, options are records with ADT fields, and all types have `Eq`, `Ord`, and `Show` instances.

## Choosing the right type

Temporal has many types because different use cases need different semantics. Picking the wrong one leads to subtle bugs.

| Use case | Type | Why |
|---|---|---|
| Timestamps, event logs, "when did this happen" | `Instant` | Unambiguous point on the UTC timeline |
| Scheduling a future meeting | `ZonedDateTime` | Preserves wall-clock time if DST rules change |
| Birthdays, holidays, "what date" | `PlainDate` | No time-of-day, no timezone to go wrong |
| Alarm clock, "at what time" | `PlainTime` | No date, wraps at midnight |
| Form inputs, display without timezone | `PlainDateTime` | Date+time, timezone tracked elsewhere |
| "March 2024", credit card expiry | `PlainYearMonth` | No day component |
| "December 25", annual recurrence | `PlainMonthDay` | No year component |
| "How long between A and B" | `Duration` | Spans from years down to nanoseconds |

**Rule of thumb**: use `Instant` for the past, `ZonedDateTime` for the future, and `PlainDate`/`PlainTime` when timezone is irrelevant. Never store a birthday as an `Instant` — timezone conversions will shift the date.

## Types

| Module | Type | Represents |
|---|---|---|
| `Temporal.Instant` | `Instant` | Exact moment on the UTC timeline (nanosecond precision) |
| `Temporal.ZonedDateTime` | `ZonedDateTime` | Wall-clock date/time in a specific time zone |
| `Temporal.PlainDateTime` | `PlainDateTime` | Calendar date and wall-clock time (no time zone) |
| `Temporal.PlainDate` | `PlainDate` | Calendar date (no time component) |
| `Temporal.PlainTime` | `PlainTime` | Wall-clock time (no date component) |
| `Temporal.PlainYearMonth` | `PlainYearMonth` | Year and month (e.g. "March 2024") |
| `Temporal.PlainMonthDay` | `PlainMonthDay` | Month and day (e.g. "December 25") |
| `Temporal.Duration` | `Duration` | Length of time (years through nanoseconds) |
| `Temporal.Now` | -- | Current time in various representations |

Each type supports construction from fields or ISO 8601 strings, property access, arithmetic with `Duration`, difference calculations (`since`/`until`), rounding, and conversions between types.

`ZonedDateTime` additionally handles DST transitions, UTC offset disambiguation, and timezone transition queries.

The module docs on Pursuit cover important gotchas per type — constrain-by-default overflow, midnight wrapping, time-only Instant arithmetic, DST disambiguation modes, and more.

## Quick examples

Date arithmetic with constrain-by-default overflow:

```purescript
import Prelude
import Data.Maybe (Maybe(..))
import Temporal.PlainDate as PD
import Temporal.Duration as D

case PD.plainDate 2024 1 31 of
  Nothing -> -- invalid date
  Just date ->
    case PD.add (D.months 1.0) date of
      Nothing -> -- overflow
      Just next -> do
        -- next is February 29, 2024 (leap year, clamped from day 31)
        PD.getMonth next -- 2
        PD.getDay next   -- 29
```

Working with time zones:

```purescript
import Temporal.Instant as I
import Temporal.ZonedDateTime as ZDT

case I.fromString "2024-03-15T14:30:00Z" of
  Nothing -> -- invalid
  Just instant -> do
    let nyc = I.toZonedDateTimeISO "America/New_York" instant
    ZDT.getHour nyc    -- 10 (UTC-4 in March)
    ZDT.getOffset nyc  -- "-04:00"
```

## Runtime requirement

Temporal is [shipping in browsers](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal#browser_compatibility) but is not yet universally available. Node.js and Bun do not have complete implementations yet. Until your target runtime supports Temporal natively, a polyfill is required at startup.

This library does **not** bundle or load the polyfill — your application must ensure `globalThis.Temporal` exists before any library code runs.

Using [`@js-temporal/polyfill`](https://github.com/nicolo-ribaudo/tc39-proposal-temporal-polyfill):

```javascript
// entry point (e.g. index.mjs)
import { Temporal } from "@js-temporal/polyfill";
globalThis.Temporal = Temporal;

// now load your PureScript application
const main = await import("./output/Main/index.js");
main.main();
```

Install the polyfill:

```
bun add -d @js-temporal/polyfill
```

Once your runtime ships Temporal natively, remove the polyfill — no changes to PureScript code needed.

## Installation

Add to your `spago.yaml` dependencies:

```yaml
package:
  dependencies:
    - temporal
```

The package depends on `prelude`, `effect`, `maybe`, and `js-bigints`.

## License

Apache 2.0 — see [LICENSE](LICENSE).

## Development

Requires `purs`, `spago`, and `bun`.

```
bun install                                        # install JS polyfill
spago build                                        # compile
NODE_OPTIONS="--import=./polyfill.mjs" spago test  # run tests
```

Tests include both example-based specs and QuickCheck property tests verifying algebraic laws (round-trip serialization, arithmetic inverses, comparison reflexivity, etc.) across thousands of random inputs.
