


function lshiftBuffer(input) {
    var asTwoBytes = input.map(function (v, i, b) {
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



function isInteger(value) {
    return typeof value === "number" && Math.floor(value) === value;
}


const Binary = {
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
    floatBE: parseBufferFor("floatBE", 4).map(buff => buff.readFloatBE(0)),
    floatLE: parseBufferFor("floatLE", 4).map(buff => buff.readFloatLE(0)),
    doubleBE: parseBufferFor("doubleBE", 8).map(buff => buff.readDoubleBE(0)),
    doubleLE: parseBufferFor("doubleLE", 8).map(buff => buff.readDoubleLE(0)),
};


function parseBufferFor(other, length) {
    return new Parsimmon(function (input, i) {
        ensureBuffer();
        if (i + length > input.length) {
            return makeFailure(i, length + " bytes for " + other);
        }
        return makeSuccess(i + length, input.slice(i, i + length));
    });
}

Parsimmon.Binary = Binary;


function ensureBuffer() {
    if (typeof Buffer === "undefined") {
        throw new Error(
            "Buffer global does not exist; please use webpack if you need to parse Buffers in the browser."
        );
    }
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

function bitSeqObj(namedAlignments) {
    ensureBuffer();
    var seenKeys = new Set(); ''
    var fullAlignments = namedAlignments.map(function (item) {
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

    for (const [name, align] of fullAlignments) {
        namesOnly.push(name)
        alignmentsOnly.push(align)
    }

    return bitSeq(alignmentsOnly).map(function (parsed) {
        const result = {}
        for (const i in namesOnly) {
            const name = namesOnly[i]
            if (name === null) continue;
            result[name] = parsed[i]
        }
        return result
    });
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

    return new Parsimmon(function (input, i) {
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
    return new Parsimmon(function (input, i) {
        var head = get(input, i);
        if (head === b) {
            return makeSuccess(i + 1, head);
        } else {
            return makeFailure(i, expected);
        }
    });
}


function assertValidIntegerByteLengthFor(who, length) {
    if (!isInteger(length) || length < 0 || length > 6) {
        throw new Error(who + " requires integer length in range [0, 6].");
    }
}

function uintBE(length) {
    assertValidIntegerByteLengthFor("uintBE", length);
    return parseBufferFor("uintBE(" + length + ")", length).map(function (buff) {
        return buff.readUIntBE(0, length);
    });
}

function uintLE(length) {
    assertValidIntegerByteLengthFor("uintLE", length);
    return parseBufferFor("uintLE(" + length + ")", length).map(function (buff) {
        return buff.readUIntLE(0, length);
    });
}

function intBE(length) {
    assertValidIntegerByteLengthFor("intBE", length);
    return parseBufferFor("intBE(" + length + ")", length).map(function (buff) {
        return buff.readIntBE(0, length);
    });
}

function intLE(length) {
    assertValidIntegerByteLengthFor("intLE", length);
    return parseBufferFor("intLE(" + length + ")", length).map(function (buff) {
        return buff.readIntLE(0, length);
    });
}


function toChunks(arr, chunkSize) {
    arr = arr.slice()
    const chunks = [];
    while (arr.length) {
      chunks.push(arr.splice(0, chunkSize))
    }
    return chunks
  }

  const bytesPerLine = 8;
  const bytesBefore = bytesPerLine * 5;
  const bytesAfter = bytesPerLine * 4;


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
  
  function toHex(byteValue) {
    // Prefix byte values with a `0` if they are shorter than 2 characters.
    return byteValue.toString(16).padStart(2, '0')
  }
  
  function formatGot_binary(input, error) {
    var verticalMarkerLength = 2;
    var byteLineWithErrorIndex = error.index.offset - (error.index.offset % bytesPerLine);
    var columnByteIndex = error.index.offset - byteLineWithErrorIndex;
    var byteRange = rangeFromIndexAndOffsets(byteLineWithErrorIndex, bytesBefore, bytesAfter + bytesPerLine, input.length);
    var bytes = input.slice(byteRange.from, byteRange.to);
  
  
    var lineRange = byteRangeToRange(byteRange);
    var lineWithErrorIndex = byteLineWithErrorIndex / bytesPerLine;
  
    if (columnByteIndex >= 4) {
      // Account for an extra space.
      var column = columnByteIndex * 3 + 1;
    } else {
      var column = columnByteIndex * 3;
    }
  
    var lines = toChunks(bytes.toJSON().data, bytesPerLine)
      .map((byteRow) => {
        const byteLine = byteRow.map(toHex)
        if (byteLine.length <= 4) {
          return byteLine.join(" ")
        } else {
          return byteLine.slice(0, 4).join(" ") + "  " + byteLine.slice(4).join(" ");
        }
      });
  
    var lastLineNumberLabelLength = (
      (lineRange.to > 0 ? lineRange.to - 1 : lineRange.to) * 8
    ).toString(16).length;
  
    if (lastLineNumberLabelLength < 2) {
      lastLineNumberLabelLength = 2;
    }
  
    return {
      verticalMarkerLength,
      column,
      lineWithErrorIndex,
      lineRange,
      lines, 
      lastLineNumberLabelLength
    }
  }
  