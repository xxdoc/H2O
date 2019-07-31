@echo off
git add .
set /p committext=ÊäÈëCommit±êÌâ£º
git commit -m "%committext%"
git push
pause