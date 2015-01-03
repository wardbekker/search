-module(postings).

-include("search.hrl").

-export([current_doc/1, goto/2]).

current_doc(#postings{ skip_nodes = [#skip_node{ docs = [CurrentDocTuple | _] } | _]}) ->
    CurrentDocTuple.

goto(GotoDocId, Posting) ->
    gen_server:cast(goto_reporter, {gotos, 1}),
    goto(GotoDocId, Posting, 0).

goto(_GotoDocId, #postings{ skip_nodes = [] }, SkipCount) ->
    gen_server:cast(skip_reporter, {skips, SkipCount}),
    {error, no_nodes_left};
goto(GotoDocId, #postings{ skip_nodes = Nodes } = Posting, SkipCount) ->
    case goto_node(GotoDocId, Nodes, SkipCount) of
        {error, Reason, SkipCount0} ->
            gen_server:cast(skip_reporter, {skips, SkipCount0}),
            {error, Reason};
        {NewNodes, SkipCount1} ->
            [ SkipNode | RestNodes ] = NewNodes,
            case goto_doc(GotoDocId, SkipNode#skip_node.docs, SkipCount1) of
                {error, _Reason, SkipCount2} ->
                    goto(
                      GotoDocId, 
                      Posting#postings {
                        skip_nodes = RestNodes,
                        head_doc_id = undefined
                       },
                      SkipCount2
                     );
                {NewDocs, SkipCount3} ->
                    gen_server:cast(skip_reporter, {skips,SkipCount3}),
                    [ {DocId, _} | _] = NewDocs,
                    Posting#postings{
                      skip_nodes = [ SkipNode#skip_node{ docs = NewDocs } | RestNodes],
                      head_doc_id = DocId
                     }
            end
    end.

goto_node(_GotoDocId, [], SkipCount) ->
    %% nodes depleted
    {error, no_nodes_left, SkipCount};
goto_node(GotoDocId, [#skip_node { start_doc_id = StartDocId } | _] = SkipNodes, SkipCount) when GotoDocId =< StartDocId + ?SKIP_INTERVAL ->
    %% start doc id is not more than current node start doc id + skip interval
    {SkipNodes, SkipCount};
goto_node(GotoDocId, [#skip_node { docs = Docs } | RestSkipNodes], SkipCount) ->
    %% doc id is more than current node start doc_id + skio interval, advance skipnode
    goto_node(GotoDocId, RestSkipNodes, SkipCount + length(Docs)).

goto_doc(_GotoDocId, [], SkipCount) ->
    %% docs depleted
    {error, no_doc_left, SkipCount};
goto_doc(GotoDocId, [{CurrentDocId, _} | _] = Docs, SkipCount) when CurrentDocId >= GotoDocId  ->
    %% current doc id is greater or equal to goto doc id
    {Docs, SkipCount - 1};
goto_doc(GotoDocId, [_ | Docs], SkipCount) ->
    %% advance document
    goto_doc(GotoDocId, Docs, SkipCount + 1).














