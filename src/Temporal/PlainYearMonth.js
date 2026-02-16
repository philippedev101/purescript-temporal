export const plainYearMonthImpl = (just) => (nothing) => (y) => (m) => {
  try {
    return just(new Temporal.PlainYearMonth(y, m));
  } catch (_) {
    return nothing;
  }
};

export const fromStringImpl = (just) => (nothing) => (str) => {
  try {
    return just(Temporal.PlainYearMonth.from(str));
  } catch (_) {
    return nothing;
  }
};

export const getYear = (ym) => ym.year;
export const getMonth = (ym) => ym.month;
export const getMonthCode = (ym) => ym.monthCode;
export const getDaysInMonth = (ym) => ym.daysInMonth;
export const getDaysInYear = (ym) => ym.daysInYear;
export const getMonthsInYear = (ym) => ym.monthsInYear;
export const getInLeapYear = (ym) => ym.inLeapYear;

export const withImpl = (just) => (nothing) => (fields) => (pym) => {
  try {
    return just(pym.with(fields));
  } catch (_) {
    return nothing;
  }
};

export const addImpl = (just) => (nothing) => (dur) => (pym) => {
  try {
    return just(pym.add(dur));
  } catch (_) {
    return nothing;
  }
};

export const subtractImpl = (just) => (nothing) => (dur) => (pym) => {
  try {
    return just(pym.subtract(dur));
  } catch (_) {
    return nothing;
  }
};

export const sinceImpl = (opts) => (self) => (other) => self.since(other, opts);
export const untilImpl = (opts) => (self) => (other) => self.until(other, opts);

export const toPlainDateImpl = (just) => (nothing) => (day) => (pym) => {
  try {
    return just(pym.toPlainDate({ day }));
  } catch (_) {
    return nothing;
  }
};

export const toString = (ym) => ym.toString();
