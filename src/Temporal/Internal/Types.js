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
