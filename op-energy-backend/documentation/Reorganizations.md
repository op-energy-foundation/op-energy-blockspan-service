# Brief

This document describes blockchain reorganizations and how they affect `op-energy`.

# Blockchain reorganization

'Blockchain reorganization' - is the term describing concurrent discovery of the
next new `best` block.

## Straight blockchain

Assuming that we have such blockchain (defined in: blockheight, timestamp, hash):

```
{ 0, 1, "aaaa"}
{ 1, 10, "bbbb"}
{ 2, 20, "cccc"}
{ 3, 30, "dddd"}
{ 4, 40, "eeee"}
{ 5, 50, "ffff"}
```

with such chain:
1. we can get the last block by height `5`;
2. we can get the last block by hash `ffff`.

There is no reorganizations of the blockchain in this case.

## 1-block blockchain reorganization

Now suppose we have another case

```
{ 0, 1, "aaaa"}
{ 1, 10, "bbbb"}
{ 2, 20, "cccc"}
{ 3, 30, "dddd"}
{ 4, 40, "eeee"} { 4, 41, "gggg"}
{ 5, 50, "ffff"}
```

In this case blocks with hashes "eeee" and "gggg" have the same height, but the
block with hash "gggg" had been discovered after "eeee" and adopted as the `best`
block by the bitcoin nodes. This case called "blockchain reorganization".

The only way to get information about block with hash `eeee` is to query it by
hash `eeee`. Each query to get block by height `4` will return information of
block with hash `gggg`.

Transactions of the block with hash `eeee` are then moved into mempool again and
will be included in the upcoming discovered blocks, probably in block with height
5 / `ffff`.

## multiblock blockchain reorganization

Now consider the case:

```
{ 0, 1, "aaaa"}
{ 1, 10, "bbbb"}
{ 2, 20, "cccc"} { 2, 41, "gggg"}
{ 3, 30, "dddd"} { 3, 42, "iiii"}
{ 4, 40, "eeee"} { 4, 43, "jjjj"}
{ 5, 50, "ffff"}
```

In this case, we have a `stalled`/`stale` branch of blocks with hashes:

```
{ 2, 20, "cccc"}
{ 3, 30, "dddd"}
{ 4, 40, "eeee"}
```

blocks of this stalled branch will only be accessable by their hashes.

and the `live`/`current` branch:

```
{ 2, 41, "gggg"}
{ 3, 42, "iiii"}
{ 4, 43, "jjjj"}
```

# How this affects op-energy

We use block discovery to calculate outcome of guesses. The fact that network
can have a stalled branches of blocks is not a pleasant fact for `op-energy`.

In order to handle reorganizations `op-energy` we use:

1. `unconfirmed blocks`;
2. storing block hashes as part of outcome calculation.

## Unconfirmed blocks

We assume that the blocks `[ tip - 5 .. tip]` are unconfirmed and we only
start to calculate outcomes at `tip - 6` block. This way we exclude possible
blockchain reorganizations within `[ tip-5 .. tip]` blocks

## Storing block hashes as part of outcome calculation

Using `unconfirmed blocks` solves the most common issue with block reorganizations.
But, there were cases in the past when due to software issues there were a stale
branch of 51 blocks, so stale branch of 6+ blocks are not impossible.

In order to handle such rare cases, during calculation of outcomes we store
confirmed block hash as part of the outcome result.

In case of 6+ blocks stale branch discovery, stored block hash allow us to still
keep the information of the discovered block even if it became stalled. When we
witnessing such case (when the block, which had already been used as a source for
calculation of outcomes became stalled) we inform user like:

```
Hi, there can be a very rare case of stale branches of more than 6 blocks long.
This mean that the block which we used to calculate outcome is now is a part of
a stalled branch. This is very rare case which still can happend sometimes and
we can't do anything about. The result won't be recalculated and this letter is
just for informational purposes.
```

# TLDR

So, basically:

1. we don't recalculate outcomes;
2. we keep stalled blocks in the database in order to be able to get their
   data;
3. we keep block hashes participated in the outcome as multiple blocks can have
   the same height (stalled/live);
4. we inform users of a rare cases of calculation based on a stale block just
   for a sake of being transparent with a case we can't do anything about;
5. this means, that this policy is a compromise: it is impossible to clean the
   outcome results and recalculate them and have the same results: the point of
   view will affect the blockchain state. Meaning that if some of outcomes had
   been calculated by blocks which became stalled at some point, then the
   recalculation of the outcome in the future will use `live` block instead of
   stalled one. This breaks functional dependency between block and outcome:
   blockchain is not an immutable entity and can have different state depending
   on time of observation. Thus, we should not loose the outcome database as we
   shouldn't assume that we can recalculate it.

