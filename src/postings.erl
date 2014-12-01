-module(postings).

-include("search.hrl").

-export([current_doc/1, goto/2]).

%% TODO depleted postings case.
current_doc(#postings{ skip_nodes = [#skip_node{ docs = [CurrentDocTuple | _] } | _]}) ->
    CurrentDocTuple.

goto(GotoDocId, #postings{ skip_nodes = Nodes } = Posting) ->
    case goto_node(GotoDocId, Nodes) of
        {error, Reason} ->
            {error, Reason};
         NewNodes ->
            [ SkipNode | RestNodes ] = NewNodes,
            case goto_doc(GotoDocId, SkipNode#skip_node.docs) of
                {error, Reason} ->
                    {error, Reason};
                NewDocs ->
                    [ {DocId, _} | _] = NewDocs,
                    Posting#postings{
                     skip_nodes = [ SkipNode#skip_node{ docs = NewDocs } | RestNodes],
                      head_doc_id = DocId
                     }
            end
    end.
                    
goto_node(_GotoDocId, []) ->
    %% nodes depleted
    {error, no_nodes_left};
goto_node(GotoDocId, [#skip_node { start_doc_id = StartDocId } | _] = SkipNodes) when GotoDocId =< StartDocId + ?SKIP_INTERVAL ->
    %% start doc id is not more than current node start doc id + 1000
    SkipNodes;
goto_node(GotoDocId, [_ | RestSkipNodes]) ->
    %% doc id is more than current node start doc_id + 1000, advance skipnode
    goto(GotoDocId, RestSkipNodes).


goto_doc(_GotoDocId, []) ->
    %% docs depleted
    {error, no_doc_left};
goto_doc(GotoDocId, [{CurrentDocId, _} | _] = Docs) when CurrentDocId >= GotoDocId  ->
    %% current doc id is greater or equal to goto doc id
    Docs;
goto_doc(GotoDocId, [_|Docs]) ->
    %% advance document
    goto_doc(GotoDocId, Docs).








    
    
    
    
    
    
