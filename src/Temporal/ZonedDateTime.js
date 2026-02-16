export const fromEpochNanosecondsImpl = (just) => (nothing) => (ns) => (tz) => {
  try {
    return just(new Temporal.ZonedDateTime(ns, tz));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.ZonedDateTime.from(str));
  } catch (_) {
    return nothing;
  }
};

export const fromStringWithImpl = (just) => (nothing) => (opts) => (str) => {
  try {
    return just(Temporal.ZonedDateTime.from(str, opts));
  } catch (_) {
    return nothing;
  }
};

export const getYear = (zdt) => zdt.year;
export const getMonth = (zdt) => zdt.month;
export const getDay = (zdt) => zdt.day;
export const getHour = (zdt) => zdt.hour;
export const getMinute = (zdt) => zdt.minute;
export const getSecond = (zdt) => zdt.second;
export const getMillisecond = (zdt) => zdt.millisecond;
export const getMicrosecond = (zdt) => zdt.microsecond;
export const getNanosecond = (zdt) => zdt.nanosecond;
export const getDayOfWeek = (zdt) => zdt.dayOfWeek;
export const getDayOfYear = (zdt) => zdt.dayOfYear;
export const getWeekOfYear = (zdt) => zdt.weekOfYear;
export const getYearOfWeek = (zdt) => zdt.yearOfWeek;
export const getDaysInMonth = (zdt) => zdt.daysInMonth;
export const getDaysInWeek = (zdt) => zdt.daysInWeek;
export const getDaysInYear = (zdt) => zdt.daysInYear;
export const getMonthsInYear = (zdt) => zdt.monthsInYear;
export const getInLeapYear = (zdt) => zdt.inLeapYear;
export const getTimeZoneId = (zdt) => zdt.timeZoneId;
export const getOffset = (zdt) => zdt.offset;
export const getOffsetNanoseconds = (zdt) => zdt.offsetNanoseconds;
export const getEpochMilliseconds = (zdt) => zdt.epochMilliseconds;
export const getEpochNanoseconds = (zdt) => zdt.epochNanoseconds;
export const getHoursInDay = (zdt) => zdt.hoursInDay;

export const withImpl = (just) => (nothing) => (opts) => (fields) => (zdt) => {
  try {
    return just(zdt.with(fields, opts));
  } catch (_) {
    return nothing;
  }
};

export const withPlainTime = (time) => (zdt) => zdt.withPlainTime(time);
export const withTimeZone = (tz) => (zdt) => zdt.withTimeZone(tz);

export const addImpl = (just) => (nothing) => (dur) => (zdt) => {
  try {
    return just(zdt.add(dur));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (dur) => (zdt) => {
  try {
    return just(zdt.subtract(dur));
  } catch (_) {
    return nothing;
  }
};

export const sinceImpl = (opts) => (self) => (other) => self.since(other, opts);
export const untilImpl = (opts) => (self) => (other) => self.until(other, opts);
export const roundImpl = (opts) => (zdt) => zdt.round(opts);

export const startOfDay = (zdt) => zdt.startOfDay();

export const getTimeZoneTransitionImpl = (just) => (nothing) => (dir) => (zdt) => {
  const result = zdt.getTimeZoneTransition(dir);
  if (result === null) return nothing;
  return just(result);
};

export const toInstant = (zdt) => zdt.toInstant();
export const toPlainDateTime = (zdt) => zdt.toPlainDateTime();
export const toPlainDate = (zdt) => zdt.toPlainDate();
export const toPlainTime = (zdt) => zdt.toPlainTime();
export const toPlainYearMonth = (zdt) => zdt.toPlainYearMonth();
export const toPlainMonthDay = (zdt) => zdt.toPlainMonthDay();

export const toString = (zdt) => zdt.toString();
