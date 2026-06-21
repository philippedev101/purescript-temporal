// Duration
export const durationEquals = (a) => (b) => Temporal.Duration.compare(a, b) === 0;
export const durationToString = (d) => d.toString();

// Instant
export const instantCompare = (a) => (b) => Temporal.Instant.compare(a, b);
export const instantToString = (i) => i.toString();

// PlainDate
export const plainDateCompare = (a) => (b) => Temporal.PlainDate.compare(a, b);
export const plainDateToString = (d) => d.toString();

// PlainTime
export const plainTimeCompare = (a) => (b) => Temporal.PlainTime.compare(a, b);
export const plainTimeToString = (t) => t.toString();
export const unsafePlainTimeBottom = new Temporal.PlainTime(0, 0, 0, 0, 0, 0);
export const unsafePlainTimeTop = new Temporal.PlainTime(23, 59, 59, 999, 999, 999);

// PlainDateTime
export const plainDateTimeCompare = (a) => (b) => Temporal.PlainDateTime.compare(a, b);
export const plainDateTimeToString = (dt) => dt.toString();

// PlainYearMonth
export const plainYearMonthCompare = (a) => (b) => Temporal.PlainYearMonth.compare(a, b);
export const plainYearMonthToString = (ym) => ym.toString();

// PlainMonthDay
export const plainMonthDayEquals = (a) => (b) => a.equals(b);
export const plainMonthDayToString = (md) => md.toString();

// ZonedDateTime
export const zonedDateTimeCompare = (a) => (b) => Temporal.ZonedDateTime.compare(a, b);
export const zonedDateTimeToString = (zdt) => zdt.toString();

// fromString impls (for the Argonaut DecodeJson instances). Each parses an
// ISO-8601 string, returning `nothing` on any parse error.
//
// Temporal.from is strict per RFC 9557 and rejects fractional seconds with
// more than 9 digits (nanosecond precision). Producers like Haskell/aeson can
// emit up to 12 (picosecond) digits, so for the lenient JSON-decode path we
// truncate any sub-nanosecond fractional digits before parsing. This affects
// only the JSON codecs, not the strict public `fromString`.
const truncateSubNano = (s) => s.replace(/(\.\d{9})\d+/, "$1");
const parseVia = (ctor) => (just) => (nothing) => (s) => {
  try {
    return just(ctor.from(truncateSubNano(s)));
  } catch (_) {
    return nothing;
  }
};
export const durationFromStringImpl = parseVia(Temporal.Duration);
export const instantFromStringImpl = parseVia(Temporal.Instant);
export const plainDateFromStringImpl = parseVia(Temporal.PlainDate);
export const plainTimeFromStringImpl = parseVia(Temporal.PlainTime);
export const plainDateTimeFromStringImpl = parseVia(Temporal.PlainDateTime);
export const plainYearMonthFromStringImpl = parseVia(Temporal.PlainYearMonth);
export const plainMonthDayFromStringImpl = parseVia(Temporal.PlainMonthDay);
export const zonedDateTimeFromStringImpl = parseVia(Temporal.ZonedDateTime);
