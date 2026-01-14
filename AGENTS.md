1. retrie-rules.txt contains common linter rules. Always run `cat retrie-rules.txt | xargs -I{} retrie --dry-run --adhoc {}` to check them. drop `--dry-run` when you decide to apply them.
2. Other than above, always run `hlint .` for full linter suggestions, and then use `hlint --refactor <FILE>`. If the linter suggestion is too complex, use ast-grep to refactor (Also use skill!).
3. append retrie-rules.txt with new rules discovered during development.
4. use `floskell` to format code. You can use `find src -type f -name "*.hs" | xargs -I{} -P8 bash -c 'echo Formatting {}; floskell {}'` to batch format.
5. lens is a powerful tool for manipulating data types. Use skill to utilize them properly.
6. this project use Relude. Any import of Prelude or usage of Prelude.xxx functions are strictly forbidden. When other imports overlap with Relude's, use Relude ones.
7. When you want to run tests, use `stack test --fast` and increase bash timeout to 10 mins.
8. Use Chinese to answer but code in English 
