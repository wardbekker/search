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

-module(search_query).

-export([query/1, terms_query/1, profile/0]).

-include("search.hrl").

-define(TOP_K, 30). %% get top 30 scoring docs
-define(TOTAL_DOC_COUNT, 132593). %% Full test sample set, number doesn't really matter as scoring is not compared with other queries in this demo.

profile() ->
    fprof:apply(search_query, terms_query, [[<<"the">>, <<"and">>]]),
    fprof:profile(),
    fprof:analyse({dest, "outfile.fprof"}).

query(Terms) ->    
    {MicroSec, {DocRelevance, TotalResults}} = timer:tc(
                                                         fun() ->
                                                                 query_(Terms)
                                                         end
                                                        ),
    
    {DocRelevance, TotalResults, MicroSec}.

query_(Terms) ->
    %% get all documents that have relevance to search terms
    {TotalResults, RelevanceScores1} = terms_query(Terms),   
    {TopKDocIds, _} = lists:unzip(RelevanceScores1),

    %% attach the fieldvalues to the relevance records
    %% TODO: really ugly, and propably inefficient code, needs fix
    DocumentFieldValues = search_index:get_document_field_value(TopKDocIds), 
    RelevanceScores2 = lists:map(fun({DocId, Score}) ->
                                         case lists:member(DocId, TopKDocIds) of
                                             false ->
                                                 [];
                                             true ->
                                                 FieldValues1 = lists:map(fun(DocumentFieldValue) ->
                                                                                  DocId1 = DocumentFieldValue#document_field_value.document_id,
                                                                                  DocId2 = DocId,
                                                                                  case DocId1 =:= DocId2 of
                                                                                      true ->
                                                                                          [#field_value{ 
                                                                                              field_name = DocumentFieldValue#document_field_value.field_name,
                                                                                              value = DocumentFieldValue#document_field_value.value}];
                                                                                      false ->
                                                                                          []
                                                                                  end
                                                                          end,
                                                                          DocumentFieldValues
                                                                         ),
                                                 FieldValues2 = lists:flatten(FieldValues1),
                                                 #document_relevance_score{ field_value = FieldValues2, document_id= DocId, relevance_score = Score }
                                         end
                                 end,
                                 RelevanceScores1),

    RelevanceScores3 = lists:flatten(RelevanceScores2), %% remove results that are facet filtered
    {RelevanceScores3, TotalResults}.

terms_query(Terms) ->    
    TopK = 30,
    TermsStemmed = [ eporter:stem(Term) || Term <- Terms],
    Postings = search_index:get_postings(TermsStemmed),
    TotalResults = lists:sum(lists:map(fun(#postings{ total_docs = TotalDocs }) ->
                                                    TotalDocs
                                            end,
                                            Postings)),
    Results = search_daat:get_top(TopK, Postings),
    {TotalResults, lists:reverse(lists:keysort(2, Results))}.
