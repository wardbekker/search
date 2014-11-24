%% The MIT License (MIT)

%% Copyright (c) 2014 Ward Bekker <ward@wardbekker.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(search_handler).
-behaviour(cowboy_http_handler).

-include("search.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Query, _} = cowboy_req:qs_val(<<"q">>, Req, ""),
    Data = case Query of
               [] ->
                   [];
               _ ->            
                   LowerCaseQuery = list_to_binary(string:to_lower(binary_to_list(Query))),
                   Terms = binary:split(LowerCaseQuery, <<" ">>, [global, trim]),
                   {DocumentRelevanceScores, TotalResults, MicroSec} = search_query:query(Terms),
                   [
                    {document_relevance_scores, record_to_proplist(DocumentRelevanceScores)},
                    {total_results, TotalResults}, 
                    {execution_milliseconds, round(MicroSec / 1000)},
                    {the_query, Query}
                   ]
           end,         
    {ok, Body} = layout_dtl:render(Data),   
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
       Body,
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% erlydtl doesn't support records, so we need to transform (nested) records to proplist
record_to_proplist(Records) when is_list(Records) ->
    [record_to_proplist(Record) || Record <- Records];
record_to_proplist(Record) when is_record(Record, document_relevance_score) ->
    Proplist1 = lists:zip(record_info(fields, document_relevance_score), tl(tuple_to_list(Record))),
    FieldValues = record_to_proplist(Record#document_relevance_score.field_value),
    Proplist2 = proplists:delete(field_value, Proplist1),
    Proplist3 = Proplist2 ++ [{field_value, FieldValues}],
    Proplist3;    
record_to_proplist(Record) when is_record(Record, field_value) ->    
    lists:zip(record_info(fields, field_value), tl(tuple_to_list(Record))).




















