# About niconico-downloader

## Synopsis

**nd.scm** [OPTIONS] [URL ...]

## Description

niconico-downloader is a small command-line program to download videos from
niconico douga. It requires the Gauche version 0.9.3.3 or later.

## Options

    -h, --help             show usage.
    -w, --wait SECOND      waiting time in case of retry. (default=300sec)
    -l, --listfile FILE    file containing URLs to download.
    -d, --economydeny      deny economy-mode movie.
    -i, --info             download with metadata. (not implement yet.)
    -s, --shuffle          shuffle list before download.

## Handling requests
At first, configure account. Set two variable `*mail*` and `*password*` in nd.scm.

example:

    ;;: set your account
    (define *mail* "foo@example.com")
    (define *password* "barbaz")

# License

niconico-downloader is released by BSD style license. See also COPYING.
