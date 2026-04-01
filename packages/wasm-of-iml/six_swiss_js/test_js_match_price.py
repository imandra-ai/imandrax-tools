"""Run the 44 test cases against the JS-backed match_price."""

from six_swiss_js import Order, OrderBook, OrderType, match_price


def test_case_1():
    ob = OrderBook(
        buys=[Order(2, OrderType.MARKET, 2437, 2.0, 1)],
        sells=[Order(3, OrderType.QUOTE, 2436, 3.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_2():
    ob = OrderBook(
        buys=[Order(2, OrderType.MARKET, 3, 2.0, 1)],
        sells=[Order(4, OrderType.QUOTE, 3, 3.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_3():
    ob = OrderBook(
        buys=[
            Order(2, OrderType.MARKET, 3, 2.0, 1),
            Order(7, OrderType.MARKET, 8, 4.0, 9),
        ],
        sells=[Order(4, OrderType.QUOTE, 3, 3.0, 0)],
    )
    assert match_price(ob, 0.0) == 4.0


def test_case_4():
    ob = OrderBook(
        buys=[Order(2, OrderType.MARKET, 8854, 2.0, 1)],
        sells=[Order(3, OrderType.QUOTE, 8855, 3.0, 0)],
    )
    assert match_price(ob, 0.0) is None


def test_case_5():
    ob = OrderBook(
        buys=[Order(2, OrderType.MARKET, 3, 2.0, -7719)],
        sells=[Order(4, OrderType.QUOTE, 5, 3.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_6():
    ob = OrderBook(
        buys=[Order(2, OrderType.MARKET, 3, 2.0, 4)],
        sells=[Order(5, OrderType.LIMIT, 6, 3.0, 7)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_7():
    ob = OrderBook(
        buys=[Order(5, OrderType.MARKET, 6, 3.0, 7)],
        sells=[Order(2, OrderType.MARKET, 3, 2.0, 4)],
    )
    assert match_price(ob, 0.0) is None


def test_case_8():
    ob = OrderBook(
        buys=[Order(5, OrderType.MARKET, 2, 3.0, 6)],
        sells=[Order(3, OrderType.MARKET, 2, 2.0, 4)],
    )
    assert match_price(ob, 0.0) == 0.0


def test_case_9():
    ob = OrderBook(
        buys=[Order(8, OrderType.MARKET, 5, 4.0, 9)],
        sells=[
            Order(6, OrderType.MARKET, 5, 3.0, 7),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
    )
    assert match_price(ob, 0.0) == 0.0


def test_case_10():
    ob = OrderBook(
        buys=[Order(5, OrderType.MARKET, 2, 3.0, 6)],
        sells=[
            Order(3, OrderType.MARKET, 2, 2.0, 4),
            Order(7, OrderType.LIMIT, 8, 4.0, 9),
        ],
    )
    assert match_price(ob, 5.0) == 4.0


def test_case_11():
    ob = OrderBook(
        buys=[Order(5, OrderType.MARKET, 2, 3.0, 6)],
        sells=[
            Order(3, OrderType.MARKET, 2, 2.0, 4),
            Order(7, OrderType.LIMIT, 8, 4.0, 9),
        ],
    )
    assert match_price(ob, 4.0) == 4.0


def test_case_12():
    ob = OrderBook(
        buys=[
            Order(8, OrderType.MARKET, 5, 4.0, 9),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
        sells=[Order(6, OrderType.MARKET, 5, 3.0, 7)],
    )
    assert match_price(ob, 0.0) == 0.0


def test_case_13():
    ob = OrderBook(
        buys=[
            Order(11, OrderType.MARKET, 8, 5.0, 12),
            Order(5, OrderType.MARKET, 6, 3.0, 7),
        ],
        sells=[
            Order(9, OrderType.MARKET, 8, 4.0, 10),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
    )
    assert match_price(ob, 0.0) == 0.0


def test_case_14():
    ob = OrderBook(
        buys=[
            Order(8, OrderType.MARKET, 5, 4.0, 9),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
        sells=[
            Order(6, OrderType.MARKET, 5, 3.0, 7),
            Order(10, OrderType.LIMIT, 11, 5.0, 12),
        ],
    )
    assert match_price(ob, 6.0) == 5.0


def test_case_15():
    ob = OrderBook(
        buys=[
            Order(8, OrderType.MARKET, 5, 4.0, 9),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
        sells=[
            Order(6, OrderType.MARKET, 5, 3.0, 7),
            Order(10, OrderType.LIMIT, 11, 5.0, 12),
        ],
    )
    assert match_price(ob, 5.0) == 5.0


def test_case_16():
    ob = OrderBook(
        buys=[
            Order(5, OrderType.MARKET, 2, 3.0, 6),
            Order(7, OrderType.LIMIT, 8, 4.0, 9),
        ],
        sells=[Order(3, OrderType.MARKET, 2, 2.0, 4)],
    )
    assert match_price(ob, 3.0) == 4.0


def test_case_17():
    ob = OrderBook(
        buys=[
            Order(5, OrderType.MARKET, 2, 3.0, 6),
            Order(7, OrderType.LIMIT, 8, 4.0, 9),
        ],
        sells=[Order(3, OrderType.MARKET, 2, 2.0, 4)],
    )
    assert match_price(ob, 4.0) == 4.0


def test_case_18():
    ob = OrderBook(
        buys=[
            Order(8, OrderType.MARKET, 5, 4.0, 9),
            Order(10, OrderType.LIMIT, 11, 5.0, 12),
        ],
        sells=[
            Order(6, OrderType.MARKET, 5, 3.0, 7),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
    )
    assert match_price(ob, 4.0) == 5.0


def test_case_19():
    ob = OrderBook(
        buys=[
            Order(8, OrderType.MARKET, 5, 4.0, 9),
            Order(10, OrderType.LIMIT, 11, 5.0, 12),
        ],
        sells=[
            Order(6, OrderType.MARKET, 5, 3.0, 7),
            Order(2, OrderType.MARKET, 3, 2.0, 4),
        ],
    )
    assert match_price(ob, 5.0) == 5.0


def test_case_20():
    ob = OrderBook(
        buys=[
            Order(5, OrderType.MARKET, 2, 3.0, 6),
            Order(7, OrderType.LIMIT, 8, 4.0, 9),
        ],
        sells=[
            Order(3, OrderType.MARKET, 2, 2.0, 4),
            Order(10, OrderType.LIMIT, 11, 5.0, 12),
        ],
    )
    assert match_price(ob, 3.0) == 4.0


def test_case_21():
    ob = OrderBook(
        buys=[
            Order(5, OrderType.MARKET, 2, 3.0, 6),
            Order(7, OrderType.LIMIT, 8, 0.0, 9),
        ],
        sells=[
            Order(3, OrderType.MARKET, 2, 2.0, 4),
            Order(10, OrderType.LIMIT, 11, -1.0, 12),
        ],
    )
    assert match_price(ob, 0.0) == -1.0


def test_case_22():
    ob = OrderBook(
        buys=[
            Order(5, OrderType.MARKET, 2, 3.0, 6),
            Order(7, OrderType.LIMIT, 8, 0.0, 9),
        ],
        sells=[
            Order(3, OrderType.MARKET, 2, 2.0, 4),
            Order(10, OrderType.LIMIT, 11, 7719.0, 12),
        ],
    )
    assert match_price(ob, 0.0) == 0.0


def test_case_23():
    ob = OrderBook(
        buys=[Order(3, OrderType.QUOTE, 2437, 3.0, 1)],
        sells=[Order(2, OrderType.MARKET, 2438, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_24():
    ob = OrderBook(
        buys=[Order(2, OrderType.QUOTE, 3, 2.0, 1)],
        sells=[Order(4, OrderType.MARKET, 3, 3.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_25():
    ob = OrderBook(
        buys=[Order(2, OrderType.QUOTE, 3, 2.0, 1)],
        sells=[
            Order(4, OrderType.MARKET, 3, 3.0, 0),
            Order(7, OrderType.MARKET, 8, 4.0, 9),
        ],
    )
    assert match_price(ob, 0.0) == 4.0


def test_case_26():
    ob = OrderBook(
        buys=[Order(3, OrderType.QUOTE, 20653, 3.0, 1)],
        sells=[Order(2, OrderType.MARKET, 20652, 2.0, 0)],
    )
    assert match_price(ob, 0.0) is None


def test_case_27():
    ob = OrderBook(
        buys=[Order(4, OrderType.QUOTE, 5, 3.0, -7719)],
        sells=[Order(2, OrderType.MARKET, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_28():
    ob = OrderBook(
        buys=[Order(4, OrderType.QUOTE, 5, 3.0, -7719)],
        sells=[Order(2, OrderType.LIMIT, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_29():
    ob = OrderBook(
        buys=[Order(3, OrderType.QUOTE, 20653, 3.0, 1)],
        sells=[Order(2, OrderType.LIMIT, 20652, 2.0, 0)],
    )
    assert match_price(ob, 0.0) is None


def test_case_30():
    ob = OrderBook(
        buys=[Order(2, OrderType.QUOTE, 3, 2.0, 1)],
        sells=[
            Order(4, OrderType.LIMIT, 3, 3.0, 0),
            Order(7, OrderType.MARKET, 8, 4.0, 9),
        ],
    )
    assert match_price(ob, 0.0) == 4.0


def test_case_31():
    ob = OrderBook(
        buys=[Order(2, OrderType.QUOTE, 3, 2.0, 1)],
        sells=[Order(4, OrderType.LIMIT, 3, 3.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_32():
    ob = OrderBook(
        buys=[Order(3, OrderType.QUOTE, 2437, 3.0, 1)],
        sells=[Order(2, OrderType.LIMIT, 2438, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_33():
    ob = OrderBook(
        buys=[Order(4, OrderType.QUOTE, 5, 3.0, -7719)],
        sells=[Order(2, OrderType.QUOTE, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_34():
    ob = OrderBook(
        buys=[Order(4, OrderType.QUOTE, 5, 3.0, 1)],
        sells=[Order(2, OrderType.QUOTE, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_35():
    ob = OrderBook(
        buys=[Order(4, OrderType.LIMIT, 5, 3.0, -7719)],
        sells=[Order(2, OrderType.QUOTE, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_36():
    ob = OrderBook(
        buys=[Order(3, OrderType.LIMIT, 2437, 3.0, 1)],
        sells=[Order(2, OrderType.QUOTE, 2438, 2.0, 0)],
    )
    assert match_price(ob, 0.0) is None


def test_case_37():
    ob = OrderBook(
        buys=[
            Order(4, OrderType.LIMIT, 3, 3.0, 1),
            Order(7, OrderType.MARKET, 8, 4.0, 9),
        ],
        sells=[Order(2, OrderType.QUOTE, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 4.0


def test_case_38():
    ob = OrderBook(
        buys=[Order(4, OrderType.LIMIT, 3, 3.0, 1)],
        sells=[Order(2, OrderType.QUOTE, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_39():
    ob = OrderBook(
        buys=[Order(3, OrderType.LIMIT, 2437, 3.0, 1)],
        sells=[Order(2, OrderType.QUOTE, 2436, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_40():
    ob = OrderBook(
        buys=[Order(2, OrderType.LIMIT, 3, 2.0, 4)],
        sells=[Order(5, OrderType.MARKET, 6, 3.0, 7)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_41():
    ob = OrderBook(
        buys=[Order(4, OrderType.LIMIT, 5, 3.0, -7719)],
        sells=[Order(2, OrderType.LIMIT, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 3.0


def test_case_42():
    ob = OrderBook(
        buys=[Order(4, OrderType.LIMIT, 5, 3.0, 1)],
        sells=[Order(2, OrderType.LIMIT, 3, 2.0, 0)],
    )
    assert match_price(ob, 0.0) == 2.0


def test_case_43():
    ob = OrderBook(buys=[Order(2, OrderType.MARKET, 3, 2.0, 4)], sells=[])
    assert match_price(ob, 0.0) is None


def test_case_44():
    ob = OrderBook(buys=[], sells=[])
    assert match_price(ob, 0.0) is None
