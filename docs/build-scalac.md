# Build and plugin compiler flags

`Module.scalac` supplies typed `ScalacFlags` to in-repository plugin compilation. The direct build compiler,
hosted compiler, and producer-declared child compiler apply the same flags to scalac. `Deprecation` with
`FatalWarnings` therefore rejects a plugin source that uses a deprecated definition, including when compilation
happens in the child process.

Build-cache identity includes `flags.canonical` independently of the extra resource identity. Changing flags
cannot restore classes compiled under another policy. An unchanged successful request restores classes without
starting scalac; a failed compile is not stored. Hosted and child compilation use the existing plugin compile
key, which also includes canonical flags.

`histos.load.BuildScalac.bootstrapFlags` is the engine's shared build-file bootstrap policy. It remains
`ScalacFlags.empty`, preserving the existing build-file behavior rather than introducing new warning strictness.
The overloads without a flags argument delegate to that value. Loader diagnostics use the same `BuildFile.drive`
overload as direct build-file compilation. Module-specific flags replace this policy, they do not append to it.

The child request carries canonical typed flags. Missing or unknown flag tokens fail with
`PluginCompileError.Worker`; decoding does not silently substitute an empty policy. A failed child writes its
diagnostic result and preserves the failed effect, so the application runtime exits nonzero.
