#!/bin/sh

erl +A 8 +c -name search@placeholder2 -mnesia dir db_data -mnesia debug verbose -mnesia no_table_loaders 4 -mnesia dump_log_write_threshold 10000 -mnesia dc_dump_limit 20 -pa `pwd`/ebin `pwd`/deps/*/ebin  -run reloader -s search_app start 
