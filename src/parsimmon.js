"use strict";

function Parsimmon(action) {
  if (!(this instanceof Parsimmon)) {
    return new Parsimmon(action);
  }
  this._ = action;
}



function lshiftBuffer(input) {
  var asTwoBytes = input.map(function(v, i, b) {
    let x;
    if (i === b.length - 1) {
      x = Buffer.from([v, 0]).readUInt16BE(0)
    } else {
      x = b.readUInt16BE(i)
    }
    return ((x << 1) & 0xffff) >> 8
  });

  return Buffer.from(asTwoBytes);
}


function sum(numArr) {
  var sum = 0;
  for (let v of numArr) sum += v;
  return sum
}


function ensureBuffer() {
  if (typeof Buffer === "undefined") {
    throw new Error(
      "Buffer global does not exist; please use webpack if you need to parse Buffers in the browser."
    );
  }
}

function bitSeq(alignments) {
  ensureBuffer();

  let totalBits = 0;
  for (let num of alignments) {
      totalBits += num;
      if (num <= 48) continue; 
      throw new Error(num + " bit range requested exceeds 48 bit (6 byte) Number max.");
  }

  if (totalBits % 8 !== 0) {
    throw new Error(
      "The bits [" +
        alignments.join(", ") +
        "] add up to " +
        totalBits +
        " which is not an even number of bytes; the total should be divisible by 8"
    );
  }
  var bytes = totalBits / 8;

  return new Parsimmon(function(input, i) {
    if (bytes + i > input.length) {
      return makeFailure(i, bytes.toString() + " bytes");
    }

    let buf = input.slice(i, i + bytes)      

    let coll = alignments.map(function (bits) {
      let v = 0;
      for (let i = 0; i < bits; i++) {
        v = (v << 1) | (buf[0] >> 7)
        buf = lshiftBuffer(buf)
      }
      return v
    })

    return makeSuccess(i + bytes, coll)
  });
}




function bitSeqObj(namedAlignments) {
  ensureBuffer();
  var seenKeys = new Set();''
  var fullAlignments = namedAlignments.map(function(item) {
    if (!Array.isArray(item)) {
      assertNumber(item);
      return [null, item];
    }

    if (item.length !== 2) {
      throw new Error(`[${item.join(", ")}] should be length 2, got length ${item.length}`);
    }
    
    assertString(item[0]);
    assertNumber(item[1]);

    if (seenKeys.has(item[0])) 
        throw new Error("duplicate key in bitSeqObj: " + item[0]);

    seenKeys.add(item[0]);
    return item;
  })
  if (seenKeys.size === 0) {
    throw new Error(
      "bitSeqObj expects at least one named pair, got [" +
        namedAlignments.join(", ") +
        "]"
    );
  }
  var namesOnly = []
  var alignmentsOnly = []

  for (const [ name, align ] of fullAlignments) {
    namesOnly.push(name)
    alignmentsOnly.push(align)
  }

  return bitSeq(alignmentsOnly).map(function(parsed) {
    const result = {}
    for (const i in namesOnly) {
      const name = namesOnly[i]
      if (name === null) continue;
      result[name] = parsed[i]
    }
    return result
  });
}

function parseBufferFor(other, length) {
  return new Parsimmon(function(input, i) {
    ensureBuffer();
    if (i + length > input.length) {
      return makeFailure(i, length + " bytes for " + other);
    }
    return makeSuccess(i + length, input.slice(i, i + length));
  });
}

function parseBuffer(length) {
  return parseBufferFor("buffer", length).map(
    unsafe => Buffer.from(unsafe)
  );
}

function encodedString(encoding, length) {
  return parseBufferFor("string", length).map(
    buff => buff.toString(encoding)
  )
}

function isInteger(value) {
  return typeof value === "number" && Math.floor(value) === value;
}

function assertValidIntegerByteLengthFor(who, length) {
  if (!isInteger(length) || length < 0 || length > 6) {
    throw new Error(who + " requires integer length in range [0, 6].");
  }
}

function uintBE(length) {
  assertValidIntegerByteLengthFor("uintBE", length);
  return parseBufferFor("uintBE(" + length + ")", length).map(function(buff) {
    return buff.readUIntBE(0, length);
  });
}

function uintLE(length) {
  assertValidIntegerByteLengthFor("uintLE", length);
  return parseBufferFor("uintLE(" + length + ")", length).map(function(buff) {
    return buff.readUIntLE(0, length);
  });
}

function intBE(length) {
  assertValidIntegerByteLengthFor("intBE", length);
  return parseBufferFor("intBE(" + length + ")", length).map(function(buff) {
    return buff.readIntBE(0, length);
  });
}

function intLE(length) {
  assertValidIntegerByteLengthFor("intLE", length);
  return parseBufferFor("intLE(" + length + ")", length).map(function(buff) {
    return buff.readIntLE(0, length);
  });
}

function floatBE() {
  return parseBufferFor("floatBE", 4).map(function(buff) {
    return buff.readFloatBE(0);
  });
}

function floatLE() {
  return parseBufferFor("floatLE", 4).map(function(buff) {
    return buff.readFloatLE(0);
  });
}

function doubleBE() {
  return parseBufferFor("doubleBE", 8).map(function(buff) {
    return buff.readDoubleBE(0);
  });
}

function doubleLE() {
  return parseBufferFor("doubleLE", 8).map(function(buff) {
    return buff.readDoubleLE(0);
  });
}

function isParser(obj) {
  return obj instanceof Parsimmon;
}

function makeSuccess(index, value) {
  return {
    status: true,
    index,
    value,
    furthest: -1,
    expected: []
  };
}

function makeFailure(index, expected) {
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

function mergeReplies(result, last) {
  if (!last) return result;
  const { status, index, value } = result;
  const furthest = Math.max(result.furthest, last.furthest)
  const delta    = result.furthest - last.furthest;
  const newdata  = { status, index, value, furthest };

  if (delta === 0)  newdata.expected = union(result.expected, last.expected)
  if (delta  >  0)  newdata.expected = result.expected;
  if (delta  <  0)  newdata.expected = last.expected;

  return newdata
}

// Hold a simple memoize for the last value
var lastLineColumnIndex = {};
function makeLineColumnIndex(input, i) {
  if (Buffer.isBuffer(input)) {
    return { offset: i, line: -1, column: -1 };
  }
  // if we are calling this function with the same arguments as last time
  // return the memoized value to prevent expensive processing below
  if (lastLineColumnIndex.input === input && lastLineColumnIndex.i === i) {
    return lastLineColumnIndex.value;
  }
  var lines = input.slice(0, i).split("\n");
  // Note that unlike the character offset, the line and column offsets are
  // 1-based.
  var lineWeAreUpTo = lines.length;
  var columnWeAreUpTo = lines[lines.length - 1].length + 1;
  var value = {
    offset: i,
    line: lineWeAreUpTo,
    column: columnWeAreUpTo
  };
  lastLineColumnIndex.input = input;
  lastLineColumnIndex.i = i;
  lastLineColumnIndex.value = value;
  return value;
}

// Returns the sorted set union of two arrays of strings
function union(xs, ys) {
  const temp = new Set([ ...xs, ...ys ])
  return Array.from(temp).sort();
}

function assertParser(p) {
  if (!isParser(p)) {
    throw new Error("not a parser: " + p);
  }
}

function get(input, i) {
  if (typeof input === "string") {
    return input.charAt(i);
  }
  return input[i];
}

// TODO[ES5]: Switch to Array.isArray eventually.
function assertArray(x) {
  if (!Array.isArray(x)) {
    throw new Error("not an array: " + x);
  }
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

var linesBeforeStringError = 2;
var linesAfterStringError = 3;
var bytesPerLine = 8;
var bytesBefore = bytesPerLine * 5;
var bytesAfter = bytesPerLine * 4;
var defaultLinePrefix = "  ";


function toChunks(arr, chunkSize) {
  arr = arr.slice()
  const chunks = [];
  while (arr.length) {
    chunks.push(arr.splice(0, chunkSize))
  }
  return chunks
}

// Get a range of indexes including `i`-th element and `before` and `after` amount of elements from `arr`.
function rangeFromIndexAndOffsets(i, before, after, length) {
  return {
    // Guard against the negative upper bound for lines included in the output.
    from: i - before > 0 ? i - before : 0,
    to: i + after > length ? length : i + after
  };
}

function byteRangeToRange(byteRange) {
  // Exception for inputs smaller than `bytesPerLine`
  if (byteRange.from === 0 && byteRange.to === 1) {
    return { from: byteRange.from, to: byteRange.to };
  }

  return {
    from: byteRange.from / bytesPerLine,
    // Round `to`, so we don't get float if the amount of bytes is not divisible by `bytesPerLine`
    to: Math.floor(byteRange.to / bytesPerLine)
  };
}

function formatGot(input, error) {
  var index = error.index;
  var i = index.offset;

  var verticalMarkerLength = 1;
  var column;
  var lineWithErrorIndex;
  var lines;
  var lineRange;
  var lastLineNumberLabelLength;

  if (i === input.length) {
    return "Got the end of the input";
  }

  if (Buffer.isBuffer(input)) {
    var byteLineWithErrorIndex = i - (i % bytesPerLine);
    var columnByteIndex = i - byteLineWithErrorIndex;
    var byteRange = rangeFromIndexAndOffsets(
      byteLineWithErrorIndex,
      bytesBefore,
      bytesAfter + bytesPerLine,
      input.length
    );
    var bytes = input.slice(byteRange.from, byteRange.to);
    var bytesInChunks = toChunks(bytes.toJSON().data, bytesPerLine);

    var byteLines = bytesInChunks.map(function(byteRow) {
      return byteRow.map(function(byteValue) {
        // Prefix byte values with a `0` if they are shorter than 2 characters.
        return byteValue.toString(16).padStart(2, '0')
      });
    });

    lineRange = byteRangeToRange(byteRange);
    lineWithErrorIndex = byteLineWithErrorIndex / bytesPerLine;
    column = columnByteIndex * 3;

    // Account for an extra space.
    if (columnByteIndex >= 4) {
      column += 1;
    }

    verticalMarkerLength = 2;
    lines = byteLines.map(function(byteLine) {
      return byteLine.length <= 4
        ? byteLine.join(" ")
        : byteLine.slice(0, 4).join(" ") + "  " + byteLine.slice(4).join(" ");
    });
    lastLineNumberLabelLength = (
      (lineRange.to > 0 ? lineRange.to - 1 : lineRange.to) * 8
    ).toString(16).length;

    if (lastLineNumberLabelLength < 2) {
      lastLineNumberLabelLength = 2;
    }
  } else {
    var inputLines = input.split(/\r\n|[\n\r\u2028\u2029]/);
    column = index.column - 1;
    lineWithErrorIndex = index.line - 1;
    lineRange = rangeFromIndexAndOffsets(
      lineWithErrorIndex,
      linesBeforeStringError,
      linesAfterStringError,
      inputLines.length
    );

    lines = inputLines.slice(lineRange.from, lineRange.to);
    lastLineNumberLabelLength = lineRange.to.toString().length;
  }

  var lineWithErrorCurrentIndex = lineWithErrorIndex - lineRange.from;

  if (Buffer.isBuffer(input)) {
    lastLineNumberLabelLength = (
      (lineRange.to > 0 ? lineRange.to - 1 : lineRange.to) * 8
    ).toString(16).length;

    if (lastLineNumberLabelLength < 2) {
      lastLineNumberLabelLength = 2;
    }
  }

  var linesWithLineNumbers = lines.reduce(
    function(acc, lineSource, index) {
      var isLineWithError = index === lineWithErrorCurrentIndex;
      var prefix = isLineWithError ? "> " : defaultLinePrefix;
      var lineNumberLabel;

      if (Buffer.isBuffer(input)) {
        lineNumberLabel = ((lineRange.from + index) * 8).toString(16).padStart(lastLineNumberLabelLength, "0");
      } else {
        lineNumberLabel = (lineRange.from + index + 1).toString().padStart(lastLineNumberLabelLength, " ");
      }

      return [].concat(
        acc,
        [prefix + lineNumberLabel + " | " + lineSource],
        isLineWithError
          ? [
              defaultLinePrefix +
                " ".repeat(lastLineNumberLabelLength) + " | " +
                " ".repeat(column) +
                "^".repeat(verticalMarkerLength)
            ]
          : []
      );
    },
    []
  );

  return linesWithLineNumbers.join("\n");
}


function formatExpected(expected) {
  if (expected.length === 1) return "Expected:\n\n" + expected[0];
  return "Expected one of the following: \n\n" + expected.join(", ");
}

function formatError(input, error) {
  return `\n-- PARSING FAILED ${"-".repeat(50)}\n\n${
    formatGot(input, error)
  }\n\n${
    formatExpected(error.expected)
  }\n`
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

function seq(...parsers) {
  parsers.forEach(assertParser)

  return new Parsimmon(function(input, i) {
    var result;
    var accum = new Array(parsers.length);
    for (var j = 0; j < parsers.length; j += 1) {
      result = mergeReplies(parsers[j]._(input, i), result);
      if (!result.status) return result;
      accum[j] = result.value;
      i = result.index;
    }
    return mergeReplies(makeSuccess(i, accum), result);
  });
}

function seqObj(...parsers) {
  var seenKeys = {};
  var totalKeys = 0;

  for (var j = 0; j < parsers.length; j += 1) {
    var p = parsers[j];
    if (isParser(p)) {
      continue;
    }
    if (Array.isArray(p) && p.length === 2 && typeof p[0] === "string" && isParser(p[1])) {
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
  return new Parsimmon(function(input, i) {
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
    return mergeReplies(makeSuccess(i, accum), result);
  });
}

function seqMap(...args) {
  if (args.length === 0) {
    throw new Error("seqMap needs at least one argument");
  }
  var mapper = args.pop();
  assertFunction(mapper);
  return seq.apply(null, args).map(function(results) {
    return mapper.apply(null, results);
  });
}

function createLanguage(parsers) {
  var language = {};
  for (const key of Object.getOwnPropertyNames(parsers)) {
    const parser = parsers[key]
    language[key] = lazy(() => parser(language))
  }
  return language;
}

function alt(...parsers) {
  parsers.forEach(assertParser)
  if (parsers.length === 0) {
    return fail("zero alternates");
  }
  return new Parsimmon(function(input, i) {
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

function sepBy(parser, separator) {
  // Argument asserted by sepBy1
  return sepBy1(parser, separator).or(succeed([]));
}

function sepBy1(parser, separator) {
  assertParser(parser);
  assertParser(separator);
  var pairs = separator.then(parser).many();
  return seqMap(parser, pairs, function(r, rs) {
    return [r].concat(rs);
  });
}

// -*- Core Parsing Methods -*-

Parsimmon.prototype.parse = function(input) {
  if (typeof input !== "string" && !Buffer.isBuffer(input)) {
    throw new Error(
      ".parse must be called with a string or Buffer as its argument"
    );
  }
  var result = this.skip(eof)._(input, 0);
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
};

// -*- Other Methods -*-

Parsimmon.prototype.tryParse = function(str) {
  var result = this.parse(str);
  if (result.status) {
    return result.value;
  } else {
    var msg = formatError(str, result);
    var err = new Error(msg);
    err.type = "ParsimmonError";
    err.result = result;
    throw err;
  }
};

Parsimmon.prototype.assert = function(condition, errorMessage) {
  return this.chain(v => condition(v) ? succeed(v) : fail(errorMessage));
};

Parsimmon.prototype.or = function(alternative) {
  return alt(this, alternative);
};

Parsimmon.prototype.trim = function(parser) {
  return this.wrap(parser, parser);
};

Parsimmon.prototype.wrap = function(leftParser, rightParser) {
  return seq(leftParser, this, rightParser).map(([left, middle]) => middle);
};

Parsimmon.prototype.thru = function(wrapper) {
  return wrapper(this);
};

Parsimmon.prototype.then = function(next) {
  assertParser(next);
  return seq(this, next).map(A => A[1]);
};

Parsimmon.prototype.many = function() {
  var self = this;

  return new Parsimmon(function(input, i) {
    var accum = [];
    var result = undefined;

    for (;;) {
      result = mergeReplies(self._(input, i), result);
      if (result.status) {
        if (i === result.index) {
          throw new Error(
            "infinite loop detected in .many() parser --- calling .many() on " +
              "a parser which can accept zero characters is usually the cause"
          );
        }
        i = result.index;
        accum.push(result.value);
      } else {
        return mergeReplies(makeSuccess(i, accum), result);
      }
    }
  });
};

Parsimmon.prototype.tieWith = function(separator) {
  assertString(separator);
  return this.map(function(args) {
    assertArray(args);
    args.forEach(assertString)
    return args.join(separator)
  });
};

Parsimmon.prototype.tie = function() {
  return this.map(function(args) {
    assertArray(args);
    args.forEach(assertString)
    return args.join("")
  });
};

Parsimmon.prototype.times = function(min, max = min) {
  var self = this;
  assertNumber(min);
  assertNumber(max);
  return new Parsimmon(function(input, i) {
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
    return mergeReplies(makeSuccess(i, accum), prevResult);
  });
};

Parsimmon.prototype.result = function(res) {
  return this.map(() => res);
};

Parsimmon.prototype.atMost = function(n) {
  return this.times(0, n);
};

Parsimmon.prototype.atLeast = function(n) {
  return seq(this.times(n), this.many())
        .map(([A, B]) => ([ ...A, ...B ]));
};

Parsimmon.prototype.map = function(fn) {
  assertFunction(fn);
  var self = this;
  return new Parsimmon(function(input, i) {
    var result = self._(input, i);
    if (!result.status) return result;
    const nvalue = fn(result.value);
    const nresult = makeSuccess(result.index, nvalue)
    return mergeReplies(nresult, result);
  });
};

Parsimmon.prototype.contramap = function(fn) {
  assertFunction(fn);
  var self = this;
  return new Parsimmon(function(input, i) {
    var result = self.parse(fn(input.slice(i)));
    if (!result.status) return result;
    return makeSuccess(i + input.length, result.value);
  });
};

Parsimmon.prototype.promap = function(f, g) {
  assertFunction(f);
  assertFunction(g);
  return this.contramap(f).map(g);
};

Parsimmon.prototype.skip = function(next) {
  return seq(this, next).map(results => results[0]);
};

Parsimmon.prototype.mark = function() {
  return seq(index, this, index).map(
    ([ start, value, end ]) => ({ start, value, end })
  );
};

Parsimmon.prototype.node = function(name) {
  return seq(index, this, index).map(
    ([ start, value, end ]) => ({ name, start, value, end })
  )
};

Parsimmon.prototype.sepBy = function(separator) {
  return sepBy(this, separator);
};

Parsimmon.prototype.sepBy1 = function(separator) {
  return sepBy1(this, separator);
};

Parsimmon.prototype.lookahead = function(x) {
  return this.skip(lookahead(x));
};

Parsimmon.prototype.notFollowedBy = function(x) {
  return this.skip(notFollowedBy(x));
};

Parsimmon.prototype.desc = function(expected) {
  if (!Array.isArray(expected)) {
    expected = [expected];
  }
  var self = this;
  return new Parsimmon(function(input, i) {
    var reply = self._(input, i);
    if (!reply.status) reply.expected = expected;
    return reply;
  });
};

Parsimmon.prototype.fallback = function(result) {
  return this.or(succeed(result));
};

Parsimmon.prototype.ap = function(other) {
  return seqMap(other, this, (f, x) => f(x));
};

Parsimmon.prototype.chain = function(f) {
  var self = this;
  return new Parsimmon(function(input, i) {
    var result = self._(input, i);
    if (!result.status) return result;
    var nextParser = f(result.value);
    return mergeReplies(nextParser._(input, result.index), result);
  });
};

// -*- Constructors -*-

function string(str) {
  assertString(str);
  return new Parsimmon(function(input, i) {
    const head = input.slice(i, i + str.length)
    if (head !== str)
      return makeFailure(i, `'${str}'`);
    return makeSuccess(i + str.length, str);
  });
}

function byte(b) {
  ensureBuffer();
  assertNumber(b);
  if (b > 0xff) {
    throw new Error(
      "Value specified to byte constructor (" +
        b +
        "=0x" +
        b.toString(16) +
        ") is larger in value than a single byte."
    );
  }
  var expected = (b > 0xf ? "0x" : "0x0") + b.toString(16);
  return new Parsimmon(function(input, i) {
    var head = get(input, i);
    if (head === b) {
      return makeSuccess(i + 1, head);
    } else {
      return makeFailure(i, expected);
    }
  });
}

function regexp(re, group = 0) {
  assertRegexp(re);
  assertNumber(group);

  var anchored = anchoredRegexp(re);

  return new Parsimmon(function(input, i) {
    var match = anchored.exec(input.slice(i));
    if (!match) return makeFailure(i, "" + re);
    
    if (0 <= group && group <= match.length) {
      return makeSuccess(i + match[0].length, match[group]);
    }
    return makeFailure(i, `valid match group (0 to ${match.length}) in ${re}`);
  });
}

function succeed(value) {
  return new Parsimmon(function(input, i) {
    return makeSuccess(i, value);
  });
}

function fail(expected) {
  return new Parsimmon(function(input, i) {
    return makeFailure(i, expected);
  });
}

function lookahead(x) {
  if (isParser(x)) {
    return new Parsimmon(function(input, i) {
      var result = x._(input, i);
      result.index = i;
      result.value = "";
      return result;
    });
  } else if (typeof x === "string") {
    return lookahead(string(x));
  } else if (x instanceof RegExp) {
    return lookahead(regexp(x));
  }
  throw new Error("not a string, regexp, or parser: " + x);
}

function notFollowedBy(parser) {
  assertParser(parser);
  return new Parsimmon(function(input, i) {
    var result = parser._(input, i);
    var text = input.slice(i, result.index);
    return result.status
      ? makeFailure(i, 'not "' + text + '"')
      : makeSuccess(i, null);
  });
}

function test(predicate) {
  assertFunction(predicate);
  return new Parsimmon(function(input, i) {
    var char = get(input, i);
    if (i < input.length && predicate(char)) {
      return makeSuccess(i + 1, char);
    } else {
      return makeFailure(i, "a character/byte matching " + predicate);
    }
  });
}

function oneOf(str) {
  var expected = str.split("");
  for (var idx = 0; idx < expected.length; idx++) {
    expected[idx] = "'" + expected[idx] + "'";
  }
  return test(function(ch) {
    return str.indexOf(ch) >= 0;
  }).desc(expected);
}

function noneOf(str) {
  return test(function(ch) {
    return str.indexOf(ch) < 0;
  }).desc("none of '" + str + "'");
}

function custom(parsingFunction) {
  return new Parsimmon(parsingFunction(makeSuccess, makeFailure));
}

// TODO[ES5]: Improve error message using JSON.stringify eventually.
function range(begin, end) {
  return test(function(ch) {
    return begin <= ch && ch <= end;
  }).desc(begin + "-" + end);
}

function takeWhile(predicate) {
  assertFunction(predicate);

  return new Parsimmon(function(input, i) {
    var j = i;
    while (j < input.length && predicate(get(input, j))) {
      j++;
    }
    return makeSuccess(j, input.slice(i, j));
  });
}

function lazy(desc, f) {
  if (arguments.length < 2) {
    f = desc;
    desc = undefined;
  }

  var parser = new Parsimmon(function(input, i) {
    parser._ = f()._;
    return parser._(input, i);
  });

  if (desc) {
    return parser.desc(desc);
  } else {
    return parser;
  }
}

// -*- Fantasy Land Extras -*-

function empty() {
  return fail("fantasy-land/empty");
}

Parsimmon.prototype.concat = Parsimmon.prototype.or;
Parsimmon.prototype.empty = empty;
Parsimmon.prototype.of = succeed;
Parsimmon.prototype["fantasy-land/ap"] = Parsimmon.prototype.ap;
Parsimmon.prototype["fantasy-land/chain"] = Parsimmon.prototype.chain;
Parsimmon.prototype["fantasy-land/concat"] = Parsimmon.prototype.concat;
Parsimmon.prototype["fantasy-land/empty"] = Parsimmon.prototype.empty;
Parsimmon.prototype["fantasy-land/of"] = Parsimmon.prototype.of;
Parsimmon.prototype["fantasy-land/map"] = Parsimmon.prototype.map;

// -*- Base Parsers -*-

var index = new Parsimmon(function(input, i) {
  return makeSuccess(i, makeLineColumnIndex(input, i));
});

var any = new Parsimmon(function(input, i) {
  if (i >= input.length) {
    return makeFailure(i, "any character/byte");
  }
  return makeSuccess(i + 1, get(input, i));
});

var all = new Parsimmon(function(input, i) {
  return makeSuccess(input.length, input.slice(i));
});

var eof = new Parsimmon(function(input, i) {
  if (i < input.length) {
    return makeFailure(i, "EOF");
  }
  return makeSuccess(i, null);
});

var cr = string("\r");
var lf = string("\n");
var crlf = string("\r\n");
var newline = alt(crlf, lf, cr).desc("newline");

Parsimmon.all = all;
Parsimmon.alt = alt;
Parsimmon.any = any;
Parsimmon.cr = cr;
Parsimmon.createLanguage = createLanguage;
Parsimmon.crlf = crlf;
Parsimmon.custom = custom;
Parsimmon.digit = regexp(/[0-9]/).desc("a digit");
Parsimmon.digits = regexp(/[0-9]*/).desc("optional digits");
Parsimmon.empty = empty;
Parsimmon.end = alt(newline, eof);
Parsimmon.eof = eof;
Parsimmon.fail = fail;
Parsimmon.formatError = formatError;
Parsimmon.index = index;
Parsimmon.isParser = isParser;
Parsimmon.lazy = lazy;
Parsimmon.letter = regexp(/[a-z]/i).desc("a letter");
Parsimmon.letters = regexp(/[a-z]*/i).desc("optional letters");
Parsimmon.lf = lf;
Parsimmon.lookahead = lookahead;
Parsimmon.makeFailure = makeFailure;
Parsimmon.makeSuccess = makeSuccess;
Parsimmon.newline = newline;
Parsimmon.noneOf = noneOf;
Parsimmon.notFollowedBy = notFollowedBy;
Parsimmon.of = succeed;
Parsimmon.oneOf = oneOf;
Parsimmon.optWhitespace = regexp(/\s*/).desc("optional whitespace");
Parsimmon.Parser = Parsimmon;

Parsimmon.range = range;
Parsimmon.regex = regexp;
Parsimmon.regexp = regexp;
Parsimmon.sepBy = sepBy;
Parsimmon.sepBy1 = sepBy1;
Parsimmon.seq = seq;
Parsimmon.seqMap = seqMap;
Parsimmon.seqObj = seqObj;
Parsimmon.string = string;
Parsimmon.succeed = succeed;
Parsimmon.takeWhile = takeWhile;
Parsimmon.test = test;



Parsimmon.whitespace = regexp(/\s+/).desc("whitespace");
Parsimmon["fantasy-land/empty"] = empty;
Parsimmon["fantasy-land/of"] = succeed;

Parsimmon.Binary = {
  bitSeq: bitSeq,
  bitSeqObj: bitSeqObj,
  byte: byte,
  buffer: parseBuffer,
  encodedString: encodedString,
  uintBE: uintBE,
  uint8BE: uintBE(1),
  uint16BE: uintBE(2),
  uint32BE: uintBE(4),
  uintLE: uintLE,
  uint8LE: uintLE(1),
  uint16LE: uintLE(2),
  uint32LE: uintLE(4),
  intBE: intBE,
  int8BE: intBE(1),
  int16BE: intBE(2),
  int32BE: intBE(4),
  intLE: intLE,
  int8LE: intLE(1),
  int16LE: intLE(2),
  int32LE: intLE(4),
  floatBE: floatBE(),
  floatLE: floatLE(),
  doubleBE: doubleBE(),
  doubleLE: doubleLE()
};

module.exports = Parsimmon;
