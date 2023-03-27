import std/[tables, strutils, sequtils, options, random]
import ./utils, ./constants, ./objects, ./typedefs

randomize()

# private fields moment
# sorry, not anymore
type MarkovGenerator* = ref object
    samples*: seq[string]
    frames*: seq[string]
    model: Table[string, Table[string, int]]

iterator sampleToFrames(sample: string, asLower: bool = true): string =
    let sample = if asLower: unicodeStringToLower(sample)
      else: sample

    yield mrkvStart

    for word in sample.split(" "):
        if word == mrkvStart or word == mrkvEnd: continue
        yield word

    yield mrkvEnd

proc addSample*(generator: MarkovGenerator, sample: string, asLower: bool = true) =
    ## Adds string to samples.
    generator.samples.add(sample)

    let startIndex = generator.frames.high + 1

    for frame in sampleToFrames(sample, asLower):
        generator.frames.add(frame)

    for i in startIndex..generator.frames.len:
        if (i + 1 > generator.frames.high): break

        let currentFrame = generator.frames[i]
        let nextFrame = generator.frames[i + 1]

        if currentFrame in generator.model:
            generator.model[currentFrame].mgetOrPut(nextFrame, 0) += 1
        else:
            generator.model[currentFrame] = {nextFrame: 1}.toTable

proc addSample*(generator: MarkovGenerator, samples: seq[string]) =
    ## Adds seqence of strings to samples.
    for sample in samples:
        generator.addSample(sample)

proc getSamples*(generator: MarkovGenerator): seq[string] = generator.samples
    ## Returns all samples of generator.

proc cleanSamples*(generator: MarkovGenerator) =
    ## Removes all string from sequence of samples.
    generator.samples.setLen(0)
    generator.frames.setLen(0)
    generator.model.clear()

proc newMarkov*(samples = newSeq[string](), asLower: bool = true): MarkovGenerator =
    ## Creates an instance of Markov generator.
    result = MarkovGenerator()
    result.model = initTable[string, Table[string, int]]()

    for sample in samples:
        result.addSample(sample, asLower = asLower)

proc generate*(generator: MarkovGenerator, options = newMarkovGenerateOptions()): Option[string] =
    ## Generates a string.
    if generator.samples.len == 0:
        raise MarkovGenerateError.newException("Sequence of samples is empty")

    var begin: string
    if options.begin.isNone: begin = mrkvStart
    else: begin = mrkvStart & " " & options.begin.get

    let beginningFrames = begin.split(" ")

    for i in 0..options.attempts:
        var attemptResult = beginningFrames
        var currentFrame = attemptResult[^1]

        while currentFrame != mrkvEnd:
            if not generator.model.hasKey(currentFrame):
                raise MarkovGenerateError.newException("Not enough samples to use \"" & beginningFrames[1..^1].join(" ") & "\" as a beginning argument")

            let nextFrame = sample(toSeq(generator.model[currentFrame].keys))

            attemptResult.add(nextFrame)
            currentFrame = nextFrame

        let stringResult = attemptResult[1..^2].join(" ")

        if options.validator(stringResult) == true:
            return some stringResult

    return none string
