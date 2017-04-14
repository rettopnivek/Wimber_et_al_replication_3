REM Determine path to experiment files
set myfile=%~dp0
REM Create MATLAB command to run main script
set "str1=run('%myfile%Main_exp.m'); exit;"
matlab -automation -r %str1%