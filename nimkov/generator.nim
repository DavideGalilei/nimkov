import tables, strutils, options, random
import utils, constants, objects

randomize()

# private fields moment
type MarkovGenerator* = ref object
    samples: seq[string]
    frames: seq[string]
    model: TableRef[string, TableRef[string, int]]
    ready: bool

proc newMarkov*(samples = newSeq[string]()): MarkovGenerator =
    ## Creates an instance of Markov generator.
    result = MarkovGenerator(samples: samples, ready: false)
    result.model = newTable[string, TableRef[string, int]]()

proc addSample*(generator: MarkovGenerator, sample: string) =
    ## Add string to sequence of samples.
    generator.samples.add(sample)
    generator.ready = false

proc cleanSamples*(generator: MarkovGenerator) =
    ## Removes all string from sequence of samples.
    generator.samples.setLen(0)
    generator.frames.setLen(0)
    generator.ready = false

proc prepare*(generator: MarkovGenerator): MarkovPrepareStatuses {.discardable.} =
    ## Prepares a generator for working. Run it after initialization of Markov generator and adding new string.
    if generator.samples.len == 0: return mrkvPrepareEmptySamples
    if generator.ready == true: return mrkvPrepareReady

    generator.frames.setLen(0)

    for sample in generator.samples:
        let words = unicodeStringToLower(sample)
            .split(" ")

        generator.frames.add(mrkvStart)

        for word in words:
            if word == mrkvStart or word == mrkvEnd: continue
            generator.frames.add(word)

        generator.frames.add(mrkvEnd)

    for i in 0..generator.frames.len:
        if (i + 1 > generator.frames.high): break

        let currentFrame = generator.frames[i]
        let nextFrame = generator.frames[i + 1]

        if currentFrame in generator.model:
            if (nextFrame in generator.model[currentFrame]):
                generator.model[currentFrame][nextFrame] += 1
            else:
                generator.model[currentFrame][nextFrame] = 1
        else:
            generator.model[currentFrame] = newTable([(nextFrame, 1)])

    generator.ready = true
    return mrkvPrepareReady

proc generate*(generator: MarkovGenerator, options = newMarkovGenerateOptions()): Option[string] =
    ## Generates string.
    if not generator.ready: raise MarkovGenerateError.newException("Generator must be prepare (call MarkovGenerator.prepare)")

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

            let nextFrame = sample(generator.model[currentFrame].getRefTableKeys)

            attemptResult.add(nextFrame)
            currentFrame = nextFrame

        let stringResult = attemptResult[1..^2].join(" ")

        if options.validator(stringResult) == true:
            return some options.formatter(stringResult)

    return none string