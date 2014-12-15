# Introduction

This demo application allows you to query ~130K articles from the Stack Exchange / Stack overflow Data dump using an Erlang implementation of the WAND / max_score evaluation stategy for Top-K Queries as outlined in this paper. It was fun to translate this Java DAAT implementation to a relative speedy Erlang implementation with immutable data structures.

[Try it out the demo here](http://search.wardbekker.com:30001). It's hosted on a [1GB Single Core server from Digital Ocean](https://www.digitalocean.com/?refcode=0d0404fa1c5c).

# Demo Quickstart

1. Git clone the repository git@github.com:wardbekker/search.git
2. Gunzip the existing data files: `gunzip Posts.xml.gz`, `gunzip db_data\document_field_value.DCD.gz` and `gunzip db_data\postings.DCD.gz`
3. Start application (and dev shell) with `./shell`
4. Browse to `http://localhost:8080/` and try out a query. For example: `erlang actor`.

# Relevant links

- [WAND / max_score Paper](http://fontoura.org/papers/vldb2011.pdf)
- [Stack Exchange data dump](https://archive.org/details/stackexchange)
- [Java DAAT example implementation](https://github.com/ashaegupta/InvIndexSearch/blob/master/invindexsimsearch/src/com/invIndexSimSearch/DAAT.java)

# Demo Code Licence

 Copyright (c) 2014 Ward Bekker <ward@wardbekker.com>

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.


