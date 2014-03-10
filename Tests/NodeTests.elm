module Main where

import ElmTest.Runner.Console (runDisplay)

import Tests.TestCases (tests)

console = runDisplay tests
