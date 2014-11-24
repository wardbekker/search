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

-define(P(Var), io:format("Debug var: ~p~n", [Var])).

-record(heap, { heap :: list({integer(), integer()}), min_score :: integer()}).
-record(postings, { term :: binary(), head_doc_id :: integer(), docs :: list({integer(),integer()}), max_score :: integer(), weight :: float()}). 
-record(term_document_field_frequency, { key, term :: binary(), document_id :: binary(), field_name :: binary(), frequency :: integer()}).
-record(document_field_value, { key, document_id :: binary(), field_name :: binary(), value :: binary() }).
-record(field_value, { field_name :: binary(), value :: binary() }).
-record(document_relevance_score, { document_id :: binary(), relevance_score :: number(), field_value :: [#field_value{}] }).
