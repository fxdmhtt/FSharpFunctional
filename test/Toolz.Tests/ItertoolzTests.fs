module Toolz.Tests.ItertoolzTests

open System
open Xunit
open FSharpPlus
open FsUnit.Xunit
open FsCheck.Xunit
open Toolz

let identity x = x
let iseven x = x % 2 = 0
let isodd x = x % 2 = 1
let inc x = x + 1
let double x = 2 * x

[<Fact>]
let ``test_accumulate`` () =
    accumulate (+) [ 1; 2; 3; 4; 5 ] None |> should equal [ 1; 3; 6; 10; 15 ]
    accumulate (*) [ 1; 2; 3; 4; 5 ] None |> should equal [ 1; 2; 6; 24; 120 ]

    accumulate (+) [ 1; 2; 3; 4; 5 ] (Some -1)
    |> should equal [ -1; 0; 2; 5; 9; 14 ]

    accumulate (+) [] (Some 1) |> should equal [ 1 ]

    let binop a b =
        System.Diagnostics.Debug.Assert(false, "binop should not be called")
        0

    // start = object()
    // assert list(accumulate(binop, [], start)) == [start]
    accumulate binop [] None |> should equal ([]: int list)
// assert list(accumulate(add, [1, 2, 3], no_default2)) == [1, 3, 6]

[<Fact>]
let ``test_concat`` () =
    concat [ []; [ 1 ]; [ 2; 3 ] ] |> should equal [ 1; 2; 3 ]
    concat [ []; []; [] ] |> should equal []
// assert (list(take(5, concat([['a', 'b'], range(1000000000)]))) ==
//         ['a', 'b', 0, 1, 2])

[<Fact>]
let ``test_cons`` () =
    cons 1 [ 2; 3 ] |> should equal [ 1; 2; 3 ]

[<Fact>]
let ``test_count`` () =
    count [ 1; 2; 3 ] |> should equal 3
    count [] |> should equal 0

    count (
        seq {
            1
            2
            3
            4
        }
    )
    |> should equal 4

    count "hello" |> should equal 5
// assert count(iter('hello')) == 5

[<Fact>]
let ``test_diff`` () =
    diff [ [ 1; 2; 3 ]; [ 1; 2; 10; 100 ] ] id |> should equal [ [ 3; 10 ] ]

    diff [ [ "apples"; "bananas" ]; [ "Apples"; "Oranges" ] ] (fun (x: string) -> x.ToUpper())
    |> should equal [ [ "bananas"; "Oranges" ] ]

    // assert raises(TypeError, lambda: list(diff()))
    // assert raises(TypeError, lambda: list(diff([1, 2])))
    // assert raises(TypeError, lambda: list(diff([1, 2], 3)))
    // assert list(diff([1, 2], (1, 2), iter([1, 2]))) == []
    // assert list(diff([1, 2, 3], (1, 10, 3), iter([1, 2, 10]))) == [
    //     (2, 10, 2), (3, 3, 10)]
    diff [ [ 1; 2 ]; [ 10 ] ] id |> should equal [ [ 1; 10 ] ]
    // assert list(diff([1, 2], [10], default=None)) == [(1, 10), (2, None)]
    // # non-variadic usage
    // assert raises(TypeError, lambda: list(diff([])))
    // assert raises(TypeError, lambda: list(diff([[]])))
    // assert raises(TypeError, lambda: list(diff([[1, 2]])))
    // assert raises(TypeError, lambda: list(diff([[1, 2], 3])))
    diff [ [ 1; 2 ]; [ 1; 3 ] ] id |> should equal [ [ 2; 3 ] ]

// data1 = [{'cost': 1, 'currency': 'dollar'},
//          {'cost': 2, 'currency': 'dollar'}]

// data2 = [{'cost': 100, 'currency': 'yen'},
//          {'cost': 300, 'currency': 'yen'}]

// conversions = {'dollar': 1, 'yen': 0.01}

// def indollars(item):
//     return conversions[item['currency']] * item['cost']

// list(diff(data1, data2, key=indollars)) == [
//     ({'cost': 2, 'currency': 'dollar'}, {'cost': 300, 'currency': 'yen'})]

[<Fact>]
let ``test_drop`` () =
    drop 2 [ 10; 20; 30; 40; 50 ] |> should equal [ 30; 40; 50 ]
    drop 3 "ABCDE" |> should equal "DE"
    drop 1 [ 3; 2; 1 ] |> should equal [ 2; 1 ]

[<Fact>]
let ``test_first`` () =
    first "ABC" |> should equal 'A'
    first "ABCDE" |> should equal 'A'
    first [ 3; 2; 1 ] |> should equal 3
// assert isinstance(first({0: 'zero', 1: 'one'}), int)

[<Fact>]
let ``test_frequencies`` () =
    frequencies [ "cat"; "cat"; "ox"; "pig"; "pig"; "cat" ]
    |> should equal (Map [ ("cat", 3); ("ox", 1); ("pig", 2) ])

    frequencies [ "cat"; "pig"; "cat"; "eel"; "pig"; "dog"; "dog"; "dog" ]
    |> should equal (Map [ ("cat", 2); ("eel", 1); ("pig", 2); ("dog", 3) ])

    frequencies ([]: float list) |> should equal (Map([]: (float * int) list))

    frequencies "onomatopoeia"
    |> should
        equal
        (Map
            [ ('a', 2)
              ('e', 1)
              ('i', 1)
              ('m', 1)
              ('o', 4)
              ('n', 1)
              ('p', 1)
              ('t', 1) ])

let ``test_get`` () =
    get 1 "ABC" |> should equal 'B'
    // get [1; 2] "ABC" |> should equal ['B'; 'C']

    let phonebook =
        Map [ ("Alice", "555-1234"); ("Bob", "555-5678"); ("Charlie", "555-9999") ]

    get "Alice" phonebook |> should equal "555-1234"
    // get ["Alice"; "Bob"] phonebook |> should equal ["555-1234"; "555-5678"]
    // get ["Alice"; "Dennis"] phonebook |> should equal ["555-1234"; None]

    get 1 "ABCDE" |> should equal 'B'
    // assert list(get([1, 3], 'ABCDE')) == list('BD')
    get 'a' (Map [ ('a', 1); ('b', 2); ('c', 3) ]) |> should equal 1
// assert get(['a', 'b'], {'a': 1, 'b': 2, 'c': 3}) == (1, 2)

// assert get('foo', {}, default='bar') == 'bar'
// assert get({}, [1, 2, 3], default='bar') == 'bar'
// assert get([0, 2], 'AB', 'C') == ('A', 'C')

// assert get([0], 'AB') == ('A',)
// assert get([], 'AB') == ()

// assert raises(IndexError, lambda: get(10, 'ABC'))
// assert raises(KeyError, lambda: get(10, {'a': 1}))
// assert raises(TypeError, lambda: get({}, [1, 2, 3]))
// assert raises(TypeError, lambda: get([1, 2, 3], 1, None))
// assert raises(KeyError, lambda: get('foo', {}, default=no_default2))

[<Fact>]
let ``test_groupby`` () =
    groupby length [ "Alice"; "Bob"; "Charlie"; "Dan"; "Edith"; "Frank" ]
    |> should
        equal
        (Map
            [ (3, [ "Bob"; "Dan" ])
              (5, [ "Alice"; "Edith"; "Frank" ])
              (7, [ "Charlie" ]) ])

    groupby iseven [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    |> should equal (Map [ (false, [ 1; 3; 5; 7 ]); (true, [ 2; 4; 6; 8 ]) ])

    groupby
        (item "gender")
        [ Map [ ("name", "Alice"); ("gender", "F") ]
          Map [ ("name", "Bob"); ("gender", "M") ]
          Map [ ("name", "Charlie"); ("gender", "M") ] ]
    |> should
        equal
        (Map
            [ ("F", [ Map [ ("gender", "F"); ("name", "Alice") ] ])
              ("M",
               [ Map [ ("gender", "M"); ("name", "Bob") ]
                 Map [ ("gender", "M"); ("name", "Charlie") ] ]) ])

    groupby iseven [ 1; 2; 3; 4 ]
    |> should equal (Map [ (true, [ 2; 4 ]); (false, [ 1; 3 ]) ])

[<Fact>]
let ``test_interleave`` () =
    interleave [ [ 1; 2 ]; [ 3; 4 ] ] |> should equal [ 1; 3; 2; 4 ]
// interleave ["ABC"; "XY"] |> should equal "AXBYC"

// interleave ["ABC"; "123"] |> should equal "A1B2C3"
// assert ''.join(interleave(('ABC', '1'))) == 'A1BC'

[<Fact>]
let ``test_interpose`` () =
    interpose 0 [ 1; 2; 3; 4 ] |> toList |> should equal [ 1; 0; 2; 0; 3; 0; 4 ]
    // assert "a" == first(rest(interpose("a", range(1000000000))))
    interpose 'X' "tarzan" |> toArray |> System.String |> should equal "tXaXrXzXaXn"

    interpose 0 (Seq.replicate 4 1)
    |> toList
    |> should equal [ 1; 0; 1; 0; 1; 0; 1 ]

    interpose '.' [ 'a'; 'b'; 'c' ]
    |> toList
    |> should equal [ 'a'; '.'; 'b'; '.'; 'c' ]

[<Fact>]
let ``test_isdistinct`` () =
    isdistinct [ 1; 2; 3 ] |> should equal true
    isdistinct [ 1; 2; 1 ] |> should equal false

    isdistinct "Hello" |> should equal false
    isdistinct "World" |> should equal true

    isdistinct (
        seq {
            1
            2
            3
        }
    )
    |> should equal true

    isdistinct (
        seq {
            1
            2
            1
        }
    )
    |> should equal false

[<Fact>]
let ``test_iterate`` () =
    iterate inc 0 |> take 5 |> toList |> should equal [ 0; 1; 2; 3; 4 ]
    iterate double 1 |> take 4 |> toList |> should equal [ 1; 2; 4; 8 ]

[<Fact>]
let ``test_join`` () =
    let names = [ (1, "one"); (2, "two"); (3, "three") ]
    let fruit = [ ("apple", 1); ("orange", 1); ("banana", 2); ("coconut", 2) ]

    let add pair =
        (pair |> fst |> fst, pair |> fst |> snd, pair |> snd |> fst, pair |> snd |> snd)

    let result = join fst names snd fruit |> map add |> Set.ofSeq

    let expected =
        Set.ofList
            [ (1, "one", "apple", 1)
              (1, "one", "orange", 1)
              (2, "two", "banana", 2)
              (2, "two", "coconut", 2) ]

    result |> should equal expected

// result = set(starmap(add, join(first, names, second, fruit,
//                                left_default=no_default2,
//                                right_default=no_default2)))
// assert result == expected

[<Fact>]
let ``test_last`` () =
    last "ABC" |> should equal 'C'
    last "ABCDE" |> should equal 'E'
    last [ 3; 2; 1 ] |> should equal 1
// assert isinstance(last({0: 'zero', 1: 'one'}), int)

[<Fact>]
let ``test_mapcat`` () =
    mapcat (fun s -> map Char.ToUpper s) [ [ 'a'; 'b' ]; [ 'c'; 'd'; 'e' ] ]
    |> should equal [ 'A'; 'B'; 'C'; 'D'; 'E' ]

    mapcat identity [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
    |> should equal [ 1; 2; 3; 4; 5; 6 ]

    mapcat rev [ [ 3; 2; 1; 0 ]; [ 6; 5; 4 ]; [ 9; 8; 7 ] ] |> should equal [ 0..9 ]

    let inc i = i + 1

    mapcat (map inc) [ [ 3; 4; 5 ]; [ 6; 7; 8 ] ]
    |> should equal [ 4; 5; 6; 7; 8; 9 ]

[<Fact>]
let ``test_nth`` () =
    nth 1 "ABC" |> should equal 'B'
    nth 2 "ABCDE" |> should equal 'C'

    nth
        2
        (seq {
            'A'
            'B'
            'C'
            'D'
            'E'
        })
    |> should equal 'C'

    nth 1 [ 3; 2; 1 ] |> should equal 2
// assert nth(0, {'foo': 'bar'}) == 'foo'
// assert raises(StopIteration, lambda: nth(10, {10: 'foo'}))
// assert nth(-2, 'ABCDE') == 'D'
// assert raises(ValueError, lambda: nth(-2, iter('ABCDE')))

[<Fact>]
let ``test_partition`` () =
    partition 2 [ 1; 2; 3; 4 ] |> toList |> should equal [ [ 1; 2 ]; [ 3; 4 ] ]
    partition 2 [ 1; 2; 3; 4; 5 ] |> toList |> should equal [ [ 1; 2 ]; [ 3; 4 ] ]

    partition 3 (seq { 0..6 })
    |> toList
    |> should equal [ [ 0; 1; 2 ]; [ 3; 4; 5 ] ]
    // assert list(partition(3, range(4), pad=-1)) == [(0, 1, 2),
    //                                                 (3, -1, -1)]
    partition 2 ([]: int list) |> toList |> should equal ([]: int list list)

[<Fact>]
let ``test_partition_all`` () =
    partition_all 2 [ 1; 2; 3; 4 ] |> toList |> should equal [ [ 1; 2 ]; [ 3; 4 ] ]
    partition_all 3 [ 0..4 ] |> toList |> should equal [ [ 0; 1; 2 ]; [ 3; 4 ] ]
    partition_all 2 ([]: int list) |> toList |> should equal ([]: int list list)

// # Regression test: https://github.com/pytoolz/toolz/issues/387
// class NoCompare(object):
//     def __eq__(self, other):
//         if self.__class__ == other.__class__:
//             return True
//         raise ValueError()
// obj = NoCompare()
// result = [(obj, obj, obj, obj), (obj, obj, obj)]
// assert list(partition_all(4, [obj]*7)) == result
// assert list(partition_all(4, iter([obj]*7))) == result

[<Fact>]
let ``test_peek`` () =
    let alist = [ "Alice"; "Bob"; "Carol" ]
    let element, blist = peek alist
    element |> should equal (alist |> head)
    blist |> should equal alist

    fun () -> peek ([]: int list) |> should throw typeof<InvalidOperationException>

[<Fact>]
let ``test_peekn`` () =
    let alist = [ "Alice"; "Bob"; "Carol" ]
    let elements, blist = peekn 2 alist
    elements |> should equal (alist |> take 2 |> toList)
    blist |> should equal alist

// elements, blist = peekn(len(alist) * 4, alist)
// assert elements == alist
// assert tuple(blist) == alist

[<Fact>]
let ``test_pluck`` () =
    pluck 0 [ [ 0; 1 ]; [ 2; 3 ]; [ 4; 5 ] ] |> toList |> should equal [ 0; 2; 4 ]
// assert list(pluck([0, 1], [[0, 1, 2], [3, 4, 5]])) == [(0, 1), (3, 4)]
// assert list(pluck(1, [[0], [0, 1]], None)) == [None, 1]

// data = [{'id': 1, 'name': 'cheese'}, {'id': 2, 'name': 'pies', 'price': 1}]
// assert list(pluck('id', data)) == [1, 2]
// assert list(pluck('price', data, 0)) == [0, 1]
// assert list(pluck(['id', 'name'], data)) == [(1, 'cheese'), (2, 'pies')]
// assert list(pluck(['name'], data)) == [('cheese',), ('pies',)]
// assert list(pluck(['price', 'other'], data, 0)) == [(0, 0), (1, 0)]

// assert raises(IndexError, lambda: list(pluck(1, [[0]])))
// assert raises(KeyError, lambda: list(pluck('name', [{'id': 1}])))

// assert list(pluck(0, [[0, 1], [2, 3], [4, 5]], no_default2)) == [0, 2, 4]
// assert raises(IndexError, lambda: list(pluck(1, [[0]], no_default2)))

[<Fact>]
let ``test_random_sample`` () =
    let alist = [ 0..99 ]

    // assert list(random_sample(prob=1, seq=alist, random_state=2016)) == alist
    random_sample 1 alist |> toList |> should equal alist

// mk_rsample = lambda rs=1: list(random_sample(prob=0.1,
//                                              seq=alist,
//                                              random_state=rs))
// rsample1 = mk_rsample()
// assert rsample1 == mk_rsample()

// rsample2 = mk_rsample(1984)
// randobj = Random(1984)
// assert rsample2 == mk_rsample(randobj)

// assert rsample1 != rsample2

// assert mk_rsample(hash(object)) == mk_rsample(hash(object))
// assert mk_rsample(hash(object)) != mk_rsample(hash(object()))
// assert mk_rsample(b"a") == mk_rsample(u"a")

// assert raises(TypeError, lambda: mk_rsample([]))

[<Fact>]
let ``test_reduceby`` () =
    let data = [ 1; 2; 3; 4; 5 ]
    reduceby iseven (+) data None |> should equal (Map [ (false, 9); (true, 6) ])
    reduceby iseven (*) data None |> should equal (Map [ (false, 15); (true, 8) ])

[<Fact>]
let ``test_remove`` () =
    remove iseven [ 1; 2; 3; 4 ] |> toList |> should equal [ 1; 3 ]
    let r = remove iseven (seq { 0..4 })
    // assert type(r) is not list
    r |> toList |> should equal (filter isodd (seq { 0..4 }) |> toList)

[<Fact>]
let ``test_second`` () =
    second "ABCDE" |> should equal 'B'
    second [ 3; 2; 1 ] |> should equal 2
// assert isinstance(second({0: 'zero', 1: 'one'}), int)

[<Fact>]
let ``test_sliding_window`` () =
    sliding_window 2 [ 1; 2; 3; 4 ]
    |> toList
    |> should equal [ [| 1; 2 |]; [| 2; 3 |]; [| 3; 4 |] ]

    sliding_window 3 [ 1; 2; 3; 4 ]
    |> toList
    |> should equal [ [| 1; 2; 3 |]; [| 2; 3; 4 |] ]

[<Fact>]
let ``test_tail`` () =
    tail 2 [ 10; 20; 30; 40; 50 ] |> should equal [ 40; 50 ]
    tail 3 "ABCDE" |> should equal "CDE"
    // assert list(tail(3, iter('ABCDE'))) == list('CDE')
    tail 2 [| 3; 2; 1 |] |> should equal [| 2; 1 |]

[<Fact>]
let ``test_take_nth`` () =
    take_nth 2 [ 10; 20; 30; 40; 50 ] |> toList |> should equal [ 10; 30; 50 ]
    take_nth 2 "ABCDE" |> String.Concat |> should equal "ACE"

[<Fact>]
let ``test_topk`` () =
    topk 2 [ 1; 100; 10; 1000 ] id |> should equal [ 1000; 100 ]

    topk 2 [ "Alice"; "Bob"; "Charlie"; "Dan" ] length
    |> should equal [ "Charlie"; "Alice" ]

    topk 2 [ 4; 1; 5; 2 ] id |> should equal [ 5; 4 ]
    topk 2 [ 4; 1; 5; 2 ] ((-) 0) |> should equal [ 1; 2 ]

    topk
        2
        (seq {
            5
            1
            4
            2
        })
        ((-) 0)
    |> toList
    |> should equal [ 1; 2 ]

    topk
        2
        [ Map [ ("a", 1); ("b", 10) ]
          Map [ ("a", 2); ("b", 9) ]
          Map [ ("a", 10); ("b", 1) ]
          Map [ ("a", 9); ("b", 2) ] ]
        (item "a")
    |> should equal [ Map [ ("a", 10); ("b", 1) ]; Map [ ("a", 9); ("b", 2) ] ]

    topk
        2
        [ Map [ ("a", 1); ("b", 10) ]
          Map [ ("a", 2); ("b", 9) ]
          Map [ ("a", 10); ("b", 1) ]
          Map [ ("a", 9); ("b", 2) ] ]
        (item "b")
    |> should equal [ Map [ ("a", 1); ("b", 10) ]; Map [ ("a", 2); ("b", 9) ] ]

    topk 2 [ (0, 4); (1, 3); (2, 2); (3, 1); (4, 0) ] fst
    |> should equal [ (4, 0); (3, 1) ]

[<Fact>]
let ``test_unique`` () =
    unique [ 1; 2; 3 ] id |> toList |> should equal [ 1; 2; 3 ]
    unique [ 1; 2; 1; 3 ] id |> toList |> should equal [ 1; 2; 3 ]

    unique [ "cat"; "mouse"; "dog"; "hen" ] length
    |> toList
    |> should equal [ "cat"; "mouse" ]

    unique [ 1; 2; 3 ] iseven |> toList |> should equal [ 1; 2 ]
