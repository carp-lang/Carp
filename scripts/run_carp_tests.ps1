# Stops script if there is an error
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$PSDefaultParameterValues['*:ErrorAction']='Stop'

function exitOnError {
  param([scriptblock]$ScriptBlock)
  & @ScriptBlock
  if ($lastexitcode -ne 0) {
    exit $lastexitcode
  }
}

# TODO Add building of examples

# Actual tests (using the test suite)
Get-ChildItem -Filter test/*.carp | ForEach-Object -Process {
  exitOnError {
    echo $_.FullName
    stack exec carp "--" -x --log-memory $_.FullName
    echo ""
  }
}

# TODO Add test for empty project (with and without core)

# TODO Add tests for error messages

# Just make sure these compile
exitOnError { stack exec carp "--" ./examples/mutual_recursion.carp -b }
exitOnError { stack exec carp "--" ./examples/guessing_game.carp -b }
exitOnError { stack exec carp "--" ./examples/no_core.carp --no-core --no-profile -b }
exitOnError { stack exec carp "--" ./examples/check_malloc.carp -b }
exitOnError { stack exec carp "--" ./examples/benchmark_*.carp -b }

# Generate docs
exitOnError { stack exec carp "--" ./docs/core/generate_core_docs.carp }

echo "ALL TESTS DONE."
