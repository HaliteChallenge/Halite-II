#!/bin/sh
cd elixirbot
mix deps.get
mix escript.build
