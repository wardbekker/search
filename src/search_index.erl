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

-module(search_index).

-export([
         delete_term_document_field_frequencies/1,
         delete_document_field_values/1,
         get_term_document_field_frequencies/1,
	 get_document_field_value/1,
         persist_term_document_field_frequency/1,
         persist_document_field_value/1,
         persist_postings/1,
         get_unique_terms/0,
         get_postings/1,
         total_number_of_documents/0,
         install/0,
         dump_tables/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("search.hrl").

delete_term_document_field_frequencies(DocumentIds) ->
    Deletes=[{#term_document_field_frequency{ document_id = DocumentId, _ = '_' }, [],['$_']} || DocumentId <- DocumentIds],
    Fun = fun() ->
              List = mnesia:select(term_document_field_frequency, Deletes),
              lists:foreach(fun(X) ->
                                    mnesia:delete_object(X)
                            end, List)
          end,
    {atomic, _ } = mnesia:transaction(Fun),
    ok.

delete_document_field_values(DocumentIds) ->
    Deletes=[{#document_field_value{ document_id = DocumentId, _ = '_' }, [],['$_']} || DocumentId <- DocumentIds],
    Fun = fun() ->
              List = mnesia:select(document_field_value, Deletes),
              lists:foreach(fun(X) ->
                                    mnesia:delete_object(X)
                            end, List)
          end,
    {atomic, _ } = mnesia:transaction(Fun),
    ok.

-spec get_term_document_field_frequencies(list()) -> [[#term_document_field_frequency{}]].
get_term_document_field_frequencies(Terms) ->
    lists:map(fun(Term) ->
                      MatchSpec = [{#term_document_field_frequency{ term = Term , _ = '_' },[],['$_']}],
    
                      Trans = fun() ->
                                      mnesia:select(term_document_field_frequency, MatchSpec)
                              end,
                      {atomic, TermDocumentFieldFrequencies} = mnesia:transaction(Trans),
                      TermDocumentFieldFrequencies
              end,
              Terms).
    
-spec get_document_field_value([binary()]) -> [#document_field_value{}].
get_document_field_value(DocumentIds) -> 
    MatchSpec = [{#document_field_value{ key = DocumentId, _ = '_'},[],['$_']} || DocumentId <- DocumentIds],
    Trans = fun() ->
                    mnesia:dirty_select(document_field_value, MatchSpec)
            end,
    {atomic, DocumentFieldValues} = mnesia:transaction(Trans),
    DocumentFieldValues.
    
-spec persist_document_field_value([#document_field_value{}]) -> ok.
persist_document_field_value(DocumentFieldValues) ->
    Trans = fun() ->
                    [mnesia:write(Record) || Record <- DocumentFieldValues]
            end,
    mnesia:transaction(Trans),
    ok.
    
-spec persist_term_document_field_frequency([#term_document_field_frequency{}]) -> ok.
persist_term_document_field_frequency(TermDocumentFieldFrequencies) ->
    Trans = fun() ->
                    [mnesia:write(Record) || Record <- TermDocumentFieldFrequencies]
            end,
    mnesia:transaction(Trans),
    ok.

persist_postings(Postings) ->
    Trans = fun() ->
                    [mnesia:dirty_write(Posting) || Posting <- Postings]
            end,
    mnesia:sync_dirty(Trans),
    ok.

get_postings(Terms) ->
    Trans = fun() ->
                    [mnesia:read(postings, Term) || Term <- Terms]
            end,   
    {atomic, Postings} = mnesia:transaction(Trans),
    lists:flatten(Postings).

get_unique_terms() ->    
    { Terms, _, _ } = lists:unzip3(mnesia:dirty_all_keys(term_document_field_frequency)),
    lists:usort(Terms).
    
total_number_of_documents() ->
    132593. %% TODO, remove magic value


install() ->    
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(term_document_field_frequency, 
                        [{type, ordered_set}, {attributes, record_info(fields, term_document_field_frequency)},
                         {ram_copies, [node()]}]),
    mnesia:create_table(document_field_value, 
                        [{type, bag}, {attributes, record_info(fields, document_field_value)},
                         {ram_copies, [node()]}]),
    mnesia:create_table(postings, 
                        [{type, ordered_set}, {attributes, record_info(fields, postings)},
                         {ram_copies, [node()]}]).


dump_tables() ->
    mnesia:dump_tables([term_document_field_frequency, document_field_value, postings]).
