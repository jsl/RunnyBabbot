# Runny Babbot

The first bunny rabbit hit in Wraskell.

## Background

RunnyBabbot is a [Twitter bot](http://twitter.com/RunnyBabbot) inspired by
[Shel Silverstein's book](http://en.wikipedia.org/wiki/Runny_Babbit).

When you mention [@RunnyBabbot](http://twitter.com/RunnyBabbot) in a Tweet, he
will respond back, converting your tweet to a
[Spoonerism](http://en.wikipedia.org/wiki/Spoonerism).

## Building

RunnyBabbot is written in Haskell and the dependencies are managed in Cabal.
To install, first you must have Haskell installed in your system. You should
build the package as follows:

    cabal install --only-dependencies
    cabal configure --enable-tests
    # Optionally (but suggested), run the test suite:
    cabal test
    cabal build

This leaves you with a binary that should be in the directory:

    dist/build/runny-babbot/runny-babbot

You can also run `cabal install` after the above steps and a binary will
be put in your `~/.cabal/bin` directory.

## Usage

An instance of this software is running on a virtual server somewhere.
To run your own instance of RunnyBabbot, you will need Twitter oauth
credentials (read/write), as well as a sqlite database.

    [oauth]

    consumer_key = "your consumer key"
    consumer_secret = "your consumer secret"
    access_token = "your access token"
    access_token_secret = "your access token secret"

The sqlite database just keeps track of the tweets that RunnyBabbot
has responded to.

For the sqlite3 database, you will just need a table called `tweets`
with a schema as follows:

    CREATE TABLE tweets (id INTEGER PRIMARY KEY);

Once you have compiled RunnyBabbot and installed the dependencies as described,
you can run the bot with the following command:

    runny-babbot --dbfile=tweets.db --configfile=twitter.cfg

Where tweets.db is the path to the sqlite3 database, and twitter.cfg is the
configuration file containing your oauth credentials.

## Author

Justin Leitgeb <justin@stackbuilders.com>

## License

MIT
