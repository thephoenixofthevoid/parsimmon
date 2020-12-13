"use strict";

class Parsimmon {

  constructor(action) {
    this._ = action;
  }

  parse (input) {
    if (typeof input !== "string" && !Buffer.isBuffer(input)) {
      throw new Error(".parse must be called with a string or Buffer as its argument");
    }
    var result = this.skip(Parsimmon.eof)._(input, 0);
    if (result.status) {
      return {
        status: true,
        value: result.value
      };
    }
    return {
      status: false,
      index: makeLineColumnIndex(input, result.furthest),
      expected: result.expected
    };
  }

  tryParse(str) {
    var result = this.parse(str);
    if (result.status) {
      return result.value;
    } else {
      var msg = Parsimmon.formatError(str, result);
      var err = new Error(msg);
      err.type = "ParsimmonError";
      err.result = result;
      throw err;
    }
  }

  static toParser(x) {
    if (Parsimmon.isParser(x)) {
      return x
    } else if (typeof x === "string") {
      return Parsimmon.string(x);
    } else if (x instanceof RegExp) {
      return Parsimmon.regexp(x);
    }
    throw new Error("not a string, regexp, or parser: " + x);
  }

  static formatError(input, error) {
    const expected = error.expected;
  
    if (expected.length === 1) {
      var fmtExpected = "Expected:\n\n" + expected[0]
    } else {
      var fmtExpected = "Expected one of the following: \n\n" + expected.join(", ");
    }
  
    return `\n-- PARSING FAILED ${"-".repeat(50)}\n\n${formatGot(input, error)}\n\n${fmtExpected}\n`
  }

  static takeWhile(predicate) {
    assertFunction(predicate);
  
    return new Parsimmon(function (input, i) {
      var j = i;
      while (j < input.length && predicate(get(input, j))) {
        j++;
      }
      return Parsimmon.makeSuccess(j, input.slice(i, j));
    });
  }

  static custom(parsingFunction) {
    return new Parsimmon(
      parsingFunction(
        Parsimmon.makeSuccess, 
        Parsimmon.makeFailure
      )
    );
  }

  static makeSuccess(index, value) {
    return {
      status: true,
      index,
      value,
      furthest: -1,
      expected: []
    };
  }

  static makeFailure(index, expected) {
    if (!Array.isArray(expected)) {
      expected = [expected];
    }
    return {
      status: false,
      index: -1,
      value: null,
      furthest: index,
      expected
    };
  }

  static lazy(desc, f) {
    if (arguments.length < 2) {
      f = desc;
      desc = undefined;
    }
  
    var parser = new Parsimmon(function (input, i) {
      parser._ = f()._;
      return parser._(input, i);
    });
  
    if (desc) {
      return parser.desc(desc);
    } else {
      return parser;
    }
  }

  static isParser(obj) {
    return obj instanceof Parsimmon;
  }

  static string(str) {
    assertString(str);
    return new Parsimmon(function (input, i) {
      const head = input.slice(i, i + str.length)
      if (head !== str) {
        return Parsimmon.makeFailure(i, `'${str}'`);
      } else {
        return Parsimmon.makeSuccess(i + str.length, str);
      }
    });
  }


  static regexp(re, group = 0) {
    assertRegexp(re);
    assertNumber(group);

    var anchored = anchoredRegexp(re);

    return new Parsimmon(function (input, i) {
      var match = anchored.exec(input.slice(i));
      if (!match) return Parsimmon.makeFailure(i, "" + re);

      if (0 <= group && group <= match.length) {
        return Parsimmon.makeSuccess(i + match[0].length, match[group]);
      } else {
        return Parsimmon.makeFailure(i, `valid match group (0 to ${match.length}) in ${re}`);
      }
    });
  }

  static succeed(value) {
    return new Parsimmon(function (input, i) {
      return Parsimmon.makeSuccess(i, value);
    });
  }
  
  static fail(expected) {
    return new Parsimmon(function (input, i) {
      return Parsimmon.makeFailure(i, expected);
    });
  }
  

}




function mergeReplies(result, last) {
  if (!last) return result;
  const { status, index, value } = result;
  const furthest = Math.max(result.furthest, last.furthest)
  const delta = result.furthest - last.furthest;
  const newdata = { status, index, value, furthest };

  if (delta === 0) newdata.expected = union(result.expected, last.expected)
  if (delta > 0) newdata.expected = result.expected;
  if (delta < 0) newdata.expected = last.expected;

  return newdata
}

function makeLineColumnIndexBinary(input, i) {
  return { offset: i, line: -1, column: -1 };
}

// Hold a simple memoize for the last value
var lastLineColumnIndex = {};

function makeLineColumnIndexString(input, i) {
  // if we are calling this function with the same arguments as last time
  // return the memoized value to prevent expensive processing below
  if (lastLineColumnIndex.input === input && lastLineColumnIndex.i === i) {
    console.log("\nHit\n")
    return lastLineColumnIndex.value;
  } else {
    console.log("\nMiss\n")
  }
  var lines = input.slice(0, i).split("\n");
  // Note that unlike the character offset, the line and column offsets are
  // 1-based.
  var value = {
    offset: i,
    line: lines.length,
    column: lines[lines.length - 1].length + 1
  };
  lastLineColumnIndex.input = input;
  lastLineColumnIndex.i = i;
  lastLineColumnIndex.value = value;
  return value;
}

function makeLineColumnIndex(input, i) {
  if (Buffer.isBuffer(input)) {
    return makeLineColumnIndexBinary(input, i)
  } else {
    return makeLineColumnIndexString(input, i)
  }
}

// Returns the sorted set union of two arrays of strings
function union(xs, ys) {
  const temp = new Set([...xs, ...ys])
  return Array.from(temp).sort();
}

function get(input, i) {
  if (typeof input === "string") {
    return input.charAt(i);
  }
  return input[i];
}

function assertNumber(x) {
  if (typeof x !== "number") {
    throw new Error("not a number: " + x);
  }
}

function assertRegexp(x) {
  if (!(x instanceof RegExp)) {
    throw new Error("not a regexp: " + x);
  }
  var f = flags(x);
  for (var i = 0; i < f.length; i++) {
    var c = f.charAt(i);
    // Only allow regexp flags [imus] for now, since [g] and [y] specifically
    // mess up Parsimmon. If more non-stateful regexp flags are added in the
    // future, this will need to be revisited.
    if (c !== "i" && c !== "m" && c !== "u" && c !== "s") {
      throw new Error('unsupported regexp flag "' + c + '": ' + x);
    }
  }
}

function assertFunction(x) {
  if (typeof x !== "function") {
    throw new Error("not a function: " + x);
  }
}

function assertString(x) {
  if (typeof x !== "string") {
    throw new Error("not a string: " + x);
  }
}

// -*- Error Formatting -*-

const linesBeforeStringError = 2;
const linesAfterStringError = 3;





// Get a range of indexes including `i`-th element and `before` and `after` amount of elements from `arr`.
function rangeFromIndexAndOffsets(i, before, after, length) {
  return {
    // Guard against the negative upper bound for lines included in the output.
    from: i - before > 0 ? i - before : 0,
    to: i + after > length ? length : i + after
  };
}





function formatGot_string(input, error) {
  var verticalMarkerLength = 1;
  var inputLines = input.split(/\r\n|[\n\r\u2028\u2029]/);
  var column = error.index.column - 1;
  var lineWithErrorIndex = error.index.line - 1;
  var lineRange = rangeFromIndexAndOffsets(lineWithErrorIndex, linesBeforeStringError, linesAfterStringError, inputLines.length);
  var lines = inputLines.slice(lineRange.from, lineRange.to);
  var lastLineNumberLabelLength = lineRange.to.toString().length;

  return {
    verticalMarkerLength,
    column,
    lineWithErrorIndex,
    lineRange,
    lines, 
    lastLineNumberLabelLength
  }
}


function formatGot(input, error) {
  if (error.index.offset === input.length) {
    return "Got the end of the input";
  }

  var {
    verticalMarkerLength,
    column,
    lineWithErrorIndex,
    lineRange,
    lines, 
    lastLineNumberLabelLength
  } = formatGot_string(input, error) //Buffer.isBuffer(input) ? formatGot_binary(input, error) : formatGot_string(input, error);

  var linesWithLineNumbers = []

  const lineNumberSpace = " ".repeat(lastLineNumberLabelLength)
  const lineMarker = " ".repeat(column) + "^".repeat(verticalMarkerLength)
  const errorLine = "  " + lineNumberSpace + " | " + lineMarker;

  lines.forEach(
    function (lineSource, index) {
      const line = lineRange.from + index;

      if (Buffer.isBuffer(input)) {
        var lineNumberLabel = (line * 8).toString(16).padStart(lastLineNumberLabelLength, "0");
      } else {
        var lineNumberLabel = (line + 1).toString(10).padStart(lastLineNumberLabelLength, " ");
      }

      if (line === lineWithErrorIndex) {
        linesWithLineNumbers.push("> " + lineNumberLabel + " | " + lineSource, errorLine)
      } else {
        linesWithLineNumbers.push("  " + lineNumberLabel + " | " + lineSource)
      }
    }
  );

  return linesWithLineNumbers.join("\n");
}


function flags(re) {
  if (re.flags !== undefined) {
    return re.flags;
  }
  // legacy browser support
  return [
    re.global ? "g" : "",
    re.ignoreCase ? "i" : "",
    re.multiline ? "m" : "",
    re.unicode ? "u" : "",
    re.sticky ? "y" : ""
  ].join("");
}

function anchoredRegexp(re) {
  return RegExp("^(?:" + re.source + ")", flags(re));
}

// -*- Combinators -*-

Parsimmon.seq = function seq(...parsers) {
  parsers = parsers.map(Parsimmon.toParser)

  return new Parsimmon(function (input, i) {
    var result;
    var accum = new Array(parsers.length);
    for (var j = 0; j < parsers.length; j += 1) {
      result = mergeReplies(parsers[j]._(input, i), result);
      if (!result.status) return result;
      accum[j] = result.value;
      i = result.index;
    }
    return mergeReplies(Parsimmon.makeSuccess(i, accum), result);
  });
}

Parsimmon.seqObj = function seqObj(...parsers) {
  var seenKeys = {};
  var totalKeys = 0;

  for (var j = 0; j < parsers.length; j += 1) {
    var p = parsers[j];
    if (Parsimmon.isParser(p)) {
      continue;
    }
    if (Array.isArray(p) && p.length === 2 && typeof p[0] === "string" && Parsimmon.isParser(p[1])) {
      if (Object.prototype.hasOwnProperty.call(seenKeys, p[0])) {
        throw new Error("seqObj: duplicate key " + p[0]);
      }
      seenKeys[p[0]] = true;
      totalKeys++;
      continue;
    }
    throw new Error(
      "seqObj arguments must be parsers or [string, parser] array pairs."
    );
  }
  if (totalKeys === 0) {
    throw new Error("seqObj expects at least one named parser, found zero");
  }
  return new Parsimmon(function (input, i) {
    var result;
    var accum = {};
    for (var j = 0; j < parsers.length; j += 1) {
      var name;
      var parser;
      if (Array.isArray(parsers[j])) {
        name = parsers[j][0];
        parser = parsers[j][1];
      } else {
        name = null;
        parser = parsers[j];
      }
      result = mergeReplies(parser._(input, i), result);
      if (!result.status) {
        return result;
      }
      if (name) {
        accum[name] = result.value;
      }
      i = result.index;
    }
    return mergeReplies(Parsimmon.makeSuccess(i, accum), result);
  });
}

Parsimmon.seqMap = function seqMap(...args) {
  if (args.length === 0) {
    throw new Error("seqMap needs at least one argument");
  }
  var mapper = args.pop();
  assertFunction(mapper);
  return Parsimmon.seq.apply(null, args).map(function (results) {
    return mapper.apply(null, results);
  });
}

Parsimmon.createLanguage = function createLanguage(parsers) {
  var language = {};
  for (const key of Object.getOwnPropertyNames(parsers)) {
    const parser = parsers[key]
    language[key] = Parsimmon.lazy(() => parser(language))
  }
  return language;
}

Parsimmon.alt = function alt(...parsers) {
  parsers = parsers.map(Parsimmon.toParser)
  if (parsers.length === 0) {
    return Parsimmon.fail("zero alternates");
  }
  return new Parsimmon(function (input, i) {
    var result;
    for (var j = 0; j < parsers.length; j += 1) {
      result = mergeReplies(parsers[j]._(input, i), result);
      if (result.status) {
        return result;
      }
    }
    return result;
  });
}

Parsimmon.sepBy = sepBy;
function sepBy(parser, separator) {
  // Argument asserted by sepBy1
  return sepBy1(parser, separator).or(Parsimmon.succeed([]));
}

Parsimmon.sepBy1 = sepBy1;
function sepBy1(parser, separator) {
  parser = Parsimmon.toParser(parser)
  separator = Parsimmon.toParser(separator)

  var pairs = separator.then(parser).many();
  return Parsimmon.seqMap(parser, pairs, function (r, rs) {
    return [r].concat(rs);
  });
}

// -*- Other Methods -*-

Parsimmon.prototype.assert = function (condition, errorMessage) {
  return this.chain(v => condition(v) ? Parsimmon.succeed(v) : Parsimmon.fail(errorMessage));
};

Parsimmon.prototype.or = function (alternative) {
  return Parsimmon.alt(this, alternative);
};

Parsimmon.prototype.trim = function (parser) {
  return this.wrap(parser, parser);
};

Parsimmon.prototype.wrap = function (leftParser, rightParser) {
  return Parsimmon.seq(leftParser, this, rightParser).map(([left, middle]) => middle);
};

Parsimmon.prototype.thru = function (wrapper) {
  return wrapper(this);
};

Parsimmon.prototype.then = function (next) {
  next = Parsimmon.toParser(next)
  return Parsimmon.seq(this, next).map(A => A[1]);
};

Parsimmon.prototype.many = function () {
  var self = this;

  return new Parsimmon(function (input, i) {
    var accum = [];
    var result = undefined;

    for (; ;) {
      result = mergeReplies(self._(input, i), result);
      if (!result.status) {
        return mergeReplies(Parsimmon.makeSuccess(i, accum), result);
      }
      if (i === result.index) {
        throw new Error(
          "infinite loop detected in .many() parser --- calling .many() on " +
          "a parser which can accept zero characters is usually the cause"
        );
      }
      i = result.index;
      accum.push(result.value);
    }
  });
};

Parsimmon.prototype.tieWith = function (separator) {
  assertString(separator);
  return this.map(function (args) {
    if (!Array.isArray(args)) {
      throw new Error("not an array: " + args);
    }
    args.forEach(assertString)
    return args.join(separator)
  });
};

Parsimmon.prototype.tie = function () {
  return this.map(function (args) {
    if (!Array.isArray(args)) {
      throw new Error("not an array: " + args);
    }
    args.forEach(assertString)
    return args.join("")
  });
};

Parsimmon.prototype.times = function (min, max = min) {
  var self = this;
  assertNumber(min);
  assertNumber(max);
  return new Parsimmon(function (input, i) {
    var accum = [];
    var result = undefined;
    var prevResult = undefined;
    for (var times = 0; times < min; times += 1) {
      result = self._(input, i);
      prevResult = mergeReplies(result, prevResult);
      if (result.status) {
        i = result.index;
        accum.push(result.value);
      } else {
        return prevResult;
      }
    }
    for (; times < max; times += 1) {
      result = self._(input, i);
      prevResult = mergeReplies(result, prevResult);
      if (result.status) {
        i = result.index;
        accum.push(result.value);
      } else {
        break;
      }
    }
    return mergeReplies(Parsimmon.makeSuccess(i, accum), prevResult);
  });
};

Parsimmon.prototype.result = function (res) {
  return this.map(() => res);
};

Parsimmon.prototype.atMost = function (n) {
  return this.times(0, n);
};

Parsimmon.prototype.atLeast = function (n) {
  return Parsimmon.seq(this.times(n), this.many())
    .map(([A, B]) => ([...A, ...B]));
};

Parsimmon.prototype.map = function (fn) {
  assertFunction(fn);
  var self = this;
  return new Parsimmon(function (input, i) {
    var result = self._(input, i);
    if (!result.status) return result;
    const nvalue = fn(result.value);
    const nresult = Parsimmon.makeSuccess(result.index, nvalue)
    return mergeReplies(nresult, result);
  });
};

Parsimmon.prototype.contramap = function (fn) {
  assertFunction(fn);
  var self = this;
  return new Parsimmon(function (input, i) {
    var result = self.parse(fn(input.slice(i)));
    if (!result.status) return result;
    return Parsimmon.makeSuccess(i + input.length, result.value);
  });
};

Parsimmon.prototype.promap = function (f, g) {
  assertFunction(f);
  assertFunction(g);
  return this.contramap(f).map(g);
};

Parsimmon.prototype.skip = function (next) {
  return Parsimmon.seq(this, next).map(results => results[0]);
};

Parsimmon.prototype.mark = function () {
  return Parsimmon.seq(Parsimmon.index, this, Parsimmon.index).map(
    ([start, value, end]) => ({ start, value, end })
  );
};

Parsimmon.prototype.node = function (name) {
  return Parsimmon.seq(Parsimmon.index, this, Parsimmon.index).map(
    ([start, value, end]) => ({ name, start, value, end })
  )
};

Parsimmon.prototype.sepBy = function (separator) {
  return sepBy(this, separator);
};

Parsimmon.prototype.sepBy1 = function (separator) {
  return sepBy1(this, separator);
};

Parsimmon.prototype.lookahead = function (x) {
  return this.skip(Parsimmon.lookahead(x));
};

Parsimmon.prototype.notFollowedBy = function (x) {
  return this.skip(Parsimmon.notFollowedBy(x));
};

Parsimmon.prototype.desc = function (expected) {
  if (!Array.isArray(expected)) {
    expected = [expected];
  }
  var self = this;
  return new Parsimmon(function (input, i) {
    var reply = self._(input, i);
    if (!reply.status) reply.expected = expected;
    return reply;
  });
};

Parsimmon.prototype.fallback = function (result) {
  return this.or(Parsimmon.succeed(result));
};

Parsimmon.prototype.ap = function (other) {
  return Parsimmon.seqMap(other, this, (f, x) => f(x));
};

Parsimmon.prototype.chain = function (f) {
  var self = this;
  return new Parsimmon(function (input, i) {
    var result = self._(input, i);
    if (!result.status) return result;
    var nextParser = f(result.value);
    return mergeReplies(nextParser._(input, result.index), result);
  });
};


Parsimmon.regex = Parsimmon.regexp;





Parsimmon.lookahead = function lookahead(x) {
  x = Parsimmon.toParser(x);
  return new Parsimmon(function (input, i) {
    var result = x._(input, i);
    result.index = i;
    result.value = "";
    return result;
  });
}

Parsimmon.notFollowedBy = function notFollowedBy(parser) {
  parser = Parsimmon.toParser(parser)
  return new Parsimmon(function (input, i) {
    var result = parser._(input, i);
    var text = input.slice(i, result.index);

    if (result.status) {
      return Parsimmon.makeFailure(i, 'not "' + text + '"')
    } else {
      return Parsimmon.makeSuccess(i, null);
    }
  });
}

Parsimmon.test = function test(predicate) {
  assertFunction(predicate);
  return new Parsimmon(function (input, i) {
    var char = get(input, i);
    if (i < input.length && predicate(char)) {
      return Parsimmon.makeSuccess(i + 1, char);
    } else {
      return Parsimmon.makeFailure(i, "a character/byte matching " + predicate);
    }
  });
}

Parsimmon.oneOf = function oneOf(str) {
  var expected = str.split("").map(l => `'${l}'`)

  return Parsimmon.test(function (ch) {
    return str.indexOf(ch) >= 0;
  }).desc(expected);
}

Parsimmon.noneOf = function noneOf(str) {
  return Parsimmon.test(function (ch) {
    return str.indexOf(ch) < 0;
  }).desc("none of '" + str + "'");
}

Parsimmon.range = function range(begin, end) {
  return Parsimmon.test(function (ch) {
    return begin <= ch && ch <= end;
  }).desc(begin + "-" + end);
}



// -*- Fantasy Land Extras -*-

function empty() {
  return Parsimmon.fail("fantasy-land/empty");
}

Parsimmon.prototype.concat = Parsimmon.prototype.or;
Parsimmon.prototype.empty = empty;
Parsimmon.prototype.of = Parsimmon.succeed;
Parsimmon.prototype["fantasy-land/ap"] = Parsimmon.prototype.ap;
Parsimmon.prototype["fantasy-land/chain"] = Parsimmon.prototype.chain;
Parsimmon.prototype["fantasy-land/concat"] = Parsimmon.prototype.concat;
Parsimmon.prototype["fantasy-land/empty"] = Parsimmon.prototype.empty;
Parsimmon.prototype["fantasy-land/of"] = Parsimmon.prototype.of;
Parsimmon.prototype["fantasy-land/map"] = Parsimmon.prototype.map;

// -*- Base Parsers -*-




Parsimmon.eof = new Parsimmon(function (input, i) {
  if (i < input.length) {
    return Parsimmon.makeFailure(i, "EOF");
  } else {
    return Parsimmon.makeSuccess(i, null);
  }
});



Parsimmon.lf = Parsimmon.string("\n");
Parsimmon.cr = Parsimmon.string("\r");
Parsimmon.crlf = Parsimmon.string("\r\n");

Parsimmon.newline = Parsimmon.alt("\r\n", "\n", "\r").desc("newline");

Parsimmon.all = new Parsimmon(function (input, i) {
  return Parsimmon.makeSuccess(input.length, input.slice(i));
});
Parsimmon.any = new Parsimmon(function (input, i) {
  if (i >= input.length) {
    return Parsimmon.makeFailure(i, "any character/byte");
  } else {
    return Parsimmon.makeSuccess(i + 1, get(input, i));
  }
});

Parsimmon.digit = Parsimmon.regexp(/[0-9]/).desc("a digit");
Parsimmon.digits = Parsimmon.regexp(/[0-9]*/).desc("optional digits");
Parsimmon.empty = empty;
Parsimmon.end = Parsimmon.alt(Parsimmon.newline, Parsimmon.eof);
Parsimmon.index = new Parsimmon(function (input, i) {
  return Parsimmon.makeSuccess(i, makeLineColumnIndex(input, i));
});
Parsimmon.letter = Parsimmon.regexp(/[a-z]/i).desc("a letter");
Parsimmon.letters = Parsimmon.regexp(/[a-z]*/i).desc("optional letters");
Parsimmon.of = Parsimmon.succeed;
Parsimmon.optWhitespace = Parsimmon.regexp(/\s*/).desc("optional whitespace");
Parsimmon.Parser = Parsimmon;
Parsimmon.whitespace = Parsimmon.regexp(/\s+/).desc("whitespace");
Parsimmon["fantasy-land/empty"] = empty;
Parsimmon["fantasy-land/of"] = Parsimmon.succeed;

module.exports = Parsimmon;
