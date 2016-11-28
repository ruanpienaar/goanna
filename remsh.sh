#!/bin/bash
erl -sname remsh -setcookie goanna -remsh "goanna@`hostname`"