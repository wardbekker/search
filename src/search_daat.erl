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

-module(search_daat).

-export([get_top/2]).

-include("search.hrl").

-spec get_top(
        integer(),
        list(#postings{})
       ) -> list({integer(), integer()}).
get_top(_N, []) ->
    [];
get_top(N, Postings) ->
    {Time, Result} = timer:tc(
                       fun() ->
                               get_top(N, Postings, #heap{ heap = [], min_score = 0}, 0, 0)
                       end
                      ),    
    ?P(["Daat algo milliseconds", Time / 1000]),
    Result.


-spec get_top(
        integer(),
        list(#postings{}),
        #heap{},
        integer(), 
        integer()
       ) -> list({integer(), integer()}).
get_top(N, Postings, Heap, CurrentDocId, UpperBound) ->
    Result = next(Postings, CurrentDocId, UpperBound),
    case Result of
        {error, not_found} ->
            Heap#heap.heap;
        {{NextDocId, _Score}, NewPostings} ->
            FullScore = full_score(NextDocId, NewPostings),
            NewHeap = heap_insert(Heap, N, NextDocId, FullScore),
            Threshold = case length(NewHeap#heap.heap) < N of 
                            true ->
                                0;
                            false ->
                                NewHeap#heap.min_score
                        end,
            get_top(N, NewPostings, NewHeap, NextDocId, Threshold)
    end.

-spec heap_insert(#heap{}, integer(), integer(), integer()) -> #heap{}.
heap_insert(Heap, MaxHeapSize, DocId, FullScore) ->
    case length(Heap#heap.heap) < MaxHeapSize of
        true ->
            Heap1 = Heap#heap.heap ++ [{DocId, FullScore}],
            Heap2 = [{_,MinScore} | _Rest] = lists:keysort(2, Heap1),
            Heap#heap{ heap = Heap2, min_score = MinScore};
        false ->
            case Heap#heap.min_score > FullScore of
                true ->
                    Heap;
                false ->
                    Heap1 = lists:keydelete(
                              Heap#heap.min_score, 
                              2,
                              Heap#heap.heap
                             ) ++ [{DocId, FullScore}],
                    Heap2 = [{_, MinScore} | _Rest] = lists:keysort(2, Heap1),
                    Heap#heap{ heap = Heap2, min_score = MinScore}
                end
    end.
               
-spec full_score(integer(), list(#postings{})) -> integer().     
full_score(DocId, Postings) ->
    lists:foldl(fun(Posting, Acc) ->
                    {CurrentDocId, CurrentScore} = postings:current_doc(Posting),
                      case CurrentDocId == DocId of
                          true ->
                              Acc + CurrentScore * Posting#postings.weight;
                          false ->
                              Acc
                      end
                end,
                0,
                Postings).
                      
-spec next(
        list(#postings{}),
        integer(),
        integer()
       ) -> {error, not_found} | {{integer(), integer()}, list(#postings{})}.
next(PostingsUnsorted, CurrentDocId, UpperBound) ->
    Postings = sort_by_head(PostingsUnsorted),
    case find_pivot(Postings, UpperBound) of
        {error, not_found} -> {error, not_found};
        Pivot -> 
            {PivotDocId, PivotDocScore} = postings:current_doc(Pivot),
            [HeadPosting | _Rest] = Postings,
            HeadPostingDocId  = HeadPosting#postings.head_doc_id,
            next(HeadPostingDocId, PivotDocId, PivotDocScore, Postings, CurrentDocId, UpperBound)
    end.

next(_HeadPostingDocId, PivotDocId, _PivotDocScore, Postings, CurrentDocId, UpperBound) 
  when PivotDocId =< CurrentDocId ->
    %% pivot has already been considered, advance one of the preceding terms
    NewPostings = skip_to(Postings, CurrentDocId + 1),
    next(NewPostings, CurrentDocId, UpperBound);
next(HeadPostingDocId, PivotDocId, PivotDocScore, Postings, _CurrentDocId, _UpperBound) 
  when PivotDocId == HeadPostingDocId ->
    %% Success, all terms preceding pTerm belong to the pivot 
    {{PivotDocId, PivotDocScore}, Postings};
next(_HeadPostingDocId, PivotDocId, _PivotDocScore, Postings, CurrentDocId, UpperBound) ->
    %% not enough mass yet on pivot, advance one of the preceding terms
    NewPostings = skip_to(Postings, PivotDocId),
    next(NewPostings, CurrentDocId, UpperBound).

-spec skip_to(list(#postings{}), integer()) -> list(#postings{}).                                 
skip_to([HeadPosting | Postings], SkipToDocId) ->
    case postings:goto(SkipToDocId, HeadPosting) of
        {error, _} -> Postings; %% remove head
        NewHeadPosting -> 
            [NewHeadPosting | Postings]
    end.

-spec find_pivot(
        list(#postings{}),
        integer()
       ) -> {error, not_found} | #postings{}.
find_pivot(Postings, UpperBound) ->
    find_pivot(Postings, UpperBound, 0).

-spec find_pivot(
        list(#postings{}),
        integer(),
        integer()) -> {error, not_found} | #postings{}. 
find_pivot([], _Threshold, _TestBound) ->
    {error, not_found};
find_pivot([Posting | Postings], Threshold, TestBound) ->
    NewTestBound = TestBound + Posting#postings.weight * Posting#postings.max_score,
    find_pivot(Posting, Postings, Threshold, NewTestBound).
   
find_pivot(Posting, _Postings, Threshold, NewTestBound) when NewTestBound >= Threshold ->
    Posting;
find_pivot(_Posting, Postings, Threshold, NewTestBound) ->
    find_pivot(Postings, Threshold, NewTestBound).

-spec sort_by_head(list(#postings{})) -> list(#postings{}).
sort_by_head(Postings) ->
    lists:keysort(#postings.head_doc_id, Postings).


    
