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
%% 


%% Imports the stack exchange XML file and transforms it to an inverted index for fast searching.
%%
%% This piece is not efficient and not ready to use for a generic data intake. You have been warned.
%%
-module(search_import).

-export([stack_exchange_import/0]).

-include("search.hrl").

-define(CHUNK_SIZE, 5000).

stack_exchange_import() ->
    {ok, Data} = file:read_file("Posts2.xml"),

    %% posttypes 1 = Stack exchange Questions
    {match, Documents1} = re:run(Data, "row Id=\"(.*?)\" PostTypeId=\"(.*?)\" .*? Body=\"(.*?)\" .*? Title=\"(.*?)\".*?Tags=\"(.*?)\"", [global, {capture, all_but_first, list}]),

    %% grouped foreach
    lists:foreach(fun(Docs) ->
                          StructLists = lists:map(fun([DocumentId, PostTypeId, Body, Title, TagsEncoded]) ->
                                            Body1 = re:replace(Body, "&lt;(.*?)&gt;", " ", [global]),
                                            Title1 = re:replace(Title, "&lt;(.*?)&gt;", " ", [global]),
                                            {match, TagsDecoded1} = re:run(TagsEncoded, "&lt;(.*?)&gt;", [global, {capture, all_but_first, list}]),
                                            TagsDecoded2 = lists:map(fun(Tag) -> list_to_binary(Tag) end, TagsDecoded1),
                                            Proplist = [{<<"document_id">>, list_to_binary(DocumentId)}, {<<"title">>, list_to_binary(Title1)}, {<<"body">>, list_to_binary(Body1)}, {<<"tags">>, TagsDecoded2}, {<<"posttype">>, [list_to_binary(PostTypeId)]}],
                                            {struct, Proplist}

                                    end,
                                    Docs),
                          intake(StructLists)
                  end,
                  [ lists:sublist(Documents1, X, ?CHUNK_SIZE) || X <- lists:seq(1,length(Documents1), ?CHUNK_SIZE) ]),
    
    %% posttypes 2 = Stack exchange Answers
    {match, Documents2} = re:run(Data, "row Id=\"(.*?)\" PostTypeId=\"(.*?)\" .*? Body=\"(.*?)\" ", [global, {capture, all_but_first, list}]),

    %% grouped foreach
    lists:foreach(fun(Docs) ->
                          StructLists = lists:map(fun([DocumentId, PostTypeId, Body]) ->
                                            Body1 = re:replace(Body, "&lt;(.*?)&gt;", " ", [global]),
                                            Proplist = [{<<"document_id">>, list_to_binary(DocumentId)}, {<<"body">>, list_to_binary(Body1)}, {<<"posttype">>, [list_to_binary(PostTypeId)]}],
                                            {struct, Proplist}

                                    end,
                                    Docs),
                          intake(StructLists)
                  end,
                  [ lists:sublist(Documents2, X, ?CHUNK_SIZE) || X <- lists:seq(1,length(Documents2), ?CHUNK_SIZE) ]),
    

    mnesia:add_table_index(term_document_field_frequency, term),

    Postings = plists:map(fun(Term) ->
                                 ?P(["Creating posting for Term", Term]),
                                 [R] = search_index:get_term_document_field_frequencies(
                                         [Term]
                                        ),
                                 to_postings(
                                   R,
                                   search_index:total_number_of_documents()
                                  )
                         end,
                         search_index:get_unique_terms(),
                          {processes, 4}
                        ),

    mnesia:clear_table(term_document_field_frequency),
    search_index:persist_postings(Postings).


to_postings([], _TotalNumberOfDocuments) ->
    [];
to_postings(TermDocumentFieldFrequencies,  TotalNumberOfDocuments) ->

    [ #term_document_field_frequency{ term = Term } | _ ] = TermDocumentFieldFrequencies,

    FreqDict = lists:foldl(fun(#term_document_field_frequency{ document_id = DocId, frequency = Freq}, AccDict) ->
                                   orddict:update_counter(DocId, Freq, AccDict)
                           end,
                           orddict:new(),
                           TermDocumentFieldFrequencies),
    SortedDocs = orddict:to_list(FreqDict),

    [{_ , MaxScore} | _] = lists:reverse(lists:keysort(2, SortedDocs)),
    [{HeadDocId, _} | _] = SortedDocs,
    #postings{
       term = Term,
       skip_nodes = lists:reverse(to_skip_nodes(SortedDocs)),
       max_score = MaxScore,
       head_doc_id = HeadDocId,
       total_docs = length(SortedDocs),
       weight = math:log(TotalNumberOfDocuments / length(SortedDocs))
      }.

to_skip_nodes([HeadDoc | RestDocs]) ->
    {StartDocId, _} = HeadDoc, 
    to_skip_nodes(RestDocs, #skip_node{ start_doc_id = StartDocId, docs = [HeadDoc]}, []).

to_skip_nodes([], CurrentSkipNode, SkipNodes) ->
    %% no docs left, return skiplist\
    CurrentSkipNode1 = CurrentSkipNode#skip_node{ docs = lists:reverse(CurrentSkipNode#skip_node.docs)}, 
    [CurrentSkipNode1 | SkipNodes];
to_skip_nodes([HeadDoc | RestDocs], CurrentSkipNode, SkipNodes) ->
    {DocId, _} = HeadDoc, 
    case DocId > CurrentSkipNode#skip_node.start_doc_id + ?SKIP_INTERVAL of
        true ->
            %% new skip node
            CurrentSkipNode1 = CurrentSkipNode#skip_node{ docs = lists:reverse(CurrentSkipNode#skip_node.docs)}, 
            to_skip_nodes(RestDocs, #skip_node{ start_doc_id = DocId, docs = [HeadDoc]}, [CurrentSkipNode1 | SkipNodes]);
        false ->
            %% append to current skip node
            to_skip_nodes(RestDocs, CurrentSkipNode#skip_node{ docs = [HeadDoc | CurrentSkipNode#skip_node.docs]}, SkipNodes)
    end.


%% Example JSON doc: {
%%     "document_id" : 1,
%%     "titel" : "A brand new day",
%%     "body" : "It's a brand new day. Everybody look around 'Cause there's a reason to rejoice you see. Everybody come out. And let's commence to singing joyfully. Everybody look up. And feel the hope that we've been waiting for."
%% }
-spec intake(list()) -> ok.
intake(StructList) ->
    {DocumentFieldFrequencyAcc, DocumentFieldValueAcc} = 
        lists:foldl(fun({struct, Proplist}, {DocumentFieldFrequencyAcc, DocumentFieldValueAcc}) ->
                            DocumentId = erlang:list_to_integer(erlang:binary_to_list(proplists:get_value(<<"document_id">>, Proplist))),
                            Proplist1 = proplists:delete(<<"document_id">>, Proplist),

                            %% remove non txt or multi value props
                            Proplist2 = proplists:delete(<<"tags">>, Proplist1),
                            Proplist3 = proplists:delete(<<"posttype">>, Proplist2),

                            DocumentFieldFrequencyAcc1 = [DocumentFieldFrequencyAcc, [term_document_field_frequency_list(DocumentId, Property,Value) || {Property,Value} <- Proplist3]],
                            DocumentFieldValueAcc1 = [DocumentFieldValueAcc, [#document_field_value{ 
                                                                                 key = DocumentId,
                                                                                 document_id = DocumentId, 
                                                                                 field_name = Field,
                                                                                 value = Value
                                                                                } 
                                                                              || {Field, Value} <- Proplist3]],
                            {DocumentFieldFrequencyAcc1, DocumentFieldValueAcc1}
                    end,
                    {[],[]},
                    StructList),

    P1 = fun() -> 
                 search_index:persist_term_document_field_frequency(
                   lists:flatten(DocumentFieldFrequencyAcc)
                  )
         end,

    P2 = fun() -> search_index:persist_document_field_value(
                    lists:flatten(DocumentFieldValueAcc)
                   ) end,

    plists:foreach(fun(P) -> P() end, [P1,P2], {processes, 2}),

    ok.



-spec term_document_field_frequency_list(integer(), binary(), binary()) -> [#term_document_field_frequency{}].
term_document_field_frequency_list(DocumentId, Property, Value) ->
    TermFrequencyList = dict:fold(
                          fun(Term, Frequency, Acc) ->
                                  [Acc, #term_document_field_frequency{ key = {Term, DocumentId, Property}, term = Term, document_id = DocumentId, field_name = Property, frequency = Frequency}]
                          end,
                          [],
                          term_frequencies(Value)),
    TermFrequencyList.

-spec term_frequencies(binary()) -> dict:dict().
term_frequencies(Value) ->
    DowncasedValue = list_to_binary(clean(string:to_lower(binary_to_list(Value)))),
    Terms1 = binary:split(DowncasedValue, <<" ">>, [global, trim]),
    Terms2 = [eporter:stem(Term) || Term <- Terms1],
    TermFrequencies = 
        lists:foldl(fun
                        (<<"">>, Acc) ->
                           Acc;
                        (Term, Acc) ->
                           dict:update_counter(Term, 1, Acc)
                   end,
                    dict:new(),
                    Terms2),
    TermFrequencies.

clean("&gt;" ++ Rest) ->
    ">" ++ clean(Rest);
clean("&lt;" ++ Rest) ->
    "<" ++ clean(Rest);
clean("&quot;" ++ Rest) ->
    " " ++ clean(Rest);
clean("&#xa;" ++ Rest) ->
    " " ++ clean(Rest);
clean("." ++ Rest) ->
    " " ++ clean(Rest);
clean("," ++ Rest) ->
    " " ++ clean(Rest);
clean("?" ++ Rest) ->
    " " ++ clean(Rest);
clean("!" ++ Rest) ->
    " " ++ clean(Rest);
clean("'" ++ Rest) ->
    " " ++ clean(Rest);
clean(";" ++ Rest) ->
    " " ++ clean(Rest);
clean(":" ++ Rest) ->
    " " ++ clean(Rest);
clean("(" ++ Rest) ->
    " " ++ clean(Rest);
clean(")" ++ Rest) ->
    " " ++ clean(Rest);
clean("/" ++ Rest) ->
    " " ++ clean(Rest);
clean("=" ++ Rest) ->
    " " ++ clean(Rest);
clean("-" ++ Rest) ->
    " " ++ clean(Rest);
clean("#" ++ Rest) ->
    " " ++ clean(Rest);
clean([H | Rest]) ->
    [H | clean(Rest)];
clean([]) ->
    [].
