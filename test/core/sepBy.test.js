"use strict";

describe("Parsimmon.sepBy/sepBy1", function() {
  var chars = Parsimmon.regexp(/[a-zA-Z]+/);
  var comma = Parsimmon.string(",");
  var csvSep = Parsimmon.sepBy(chars, comma);
  var csvSep1 = Parsimmon.sepBy1(chars, comma);
  var csvSepB = chars.sepBy(comma);
  var csvSep1B = chars.sepBy1(comma);

  it("successful, returns an array of parsed elements", function() {
    var input = "Heres,a,csv,string,in,our,restrictive,dialect";
    var output = [
      "Heres",
      "a",
      "csv",
      "string",
      "in",
      "our",
      "restrictive",
      "dialect"
    ];
    assert.deepEqual(csvSep.parse(input).value, output);
    assert.deepEqual(csvSep1.parse(input).value, output);
    assert.deepEqual(csvSepB.parse(input).value, output);
    assert.deepEqual(csvSep1B.parse(input).value, output);
    assert.throws(function() {
      Parsimmon.sepBy(null); // "not a parser"
    });
    assert.throws(function() {
      Parsimmon.sepBy(Parsimmon.string("a"), null);//"not a parser"
    });
    assert.throws(function() {
      Parsimmon.string("a").sepBy(null);//"not a parser"
    });
  });

  it("sepBy succeeds with the empty list on empty input, sepBy1 fails", function() {
    assert.deepEqual(csvSep.parse("").value, []);
    assert.deepEqual(csvSepB.parse("").value, []);
    var failure = {
      status: false,
      index: {
        offset: 0,
        line: 1,
        column: 1
      },
      expected: ["/[a-zA-Z]+/"]
    };
    assert.deepEqual(csvSep1.parse(""), failure);
    assert.deepEqual(csvSep1B.parse(""), failure);
  });

  it("does not tolerate trailing separators", function() {
    var input = "Heres,a,csv,with,a,trailing,comma,";
    var output = {
      status: false,
      index: {
        offset: 34,
        line: 1,
        column: 35
      },
      expected: ["/[a-zA-Z]+/"]
    };
    assert.deepEqual(csvSep.parse(input), output);
    assert.deepEqual(csvSep1.parse(input), output);
    assert.deepEqual(csvSepB.parse(input), output);
    assert.deepEqual(csvSep1B.parse(input), output);
  });
});
