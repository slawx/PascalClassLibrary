@echo off
if not exist %1\Languages mkdir %1\Languages
copy Languages\* %1\Languages
