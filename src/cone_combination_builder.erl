-module(cone_combination_builder).
-export([cone_combinations/2, cone_combinations/1, most_popular_combinations/2]).

cone_combinations(Top_flavors, Bottom_flavors) ->
    lists:flatmap(fun(Top) ->
        lists:map(fun(Bottom) -> {Top, Bottom} end, Bottom_flavors)
    end, Top_flavors).

cone_combinations(Flavor_list) ->
    lists:flatmap(fun(Top) ->
        lists:map(fun(Bottom) -> {Top, Bottom} end, Flavor_list)
    end, Flavor_list).

most_popular_combinations(Count, List) ->
    case Count >= 0 of
        true ->
            {H, _Remainder} = lists:split(Count, lists:sort(List)),
            H;
        false ->
            fail
    end.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

cone_combinations_two_list_test_() ->
    Top_scoop_list = [vanilla, chocolate, cherry_ripple],
    Bottom_scoop_list = [lemon, butterscotch, licorice_riple],
    Short_combinations = [
        {vanilla, lemon}, {vanilla, butterscotch}, {vanilla, licorice_riple},
        {chocolate, lemon}, {chocolate, butterscotch}, {chocolate, licorice_riple},
        {cherry_ripple, lemon}, {cherry_ripple, butterscotch}, {cherry_ripple, licorice_riple}
    ],
    [?_assertEqual(Short_combinations, cone_combinations(Top_scoop_list, Bottom_scoop_list)),
     ?_assertEqual([], cone_combinations(Top_scoop_list, [])),
     ?_assertEqual([], cone_combinations([], Bottom_scoop_list)),
     ?_assertEqual([], cone_combinations([], [])),
     ?_assertEqual([], cone_combinations([], nil)),
     ?_assertEqual(fail, cone_combinations(nil, [])),
     ?_assertEqual(fail, cone_combinations(nil, nil))
    ].

cone_combinations_one_list_test_() ->
    Flavor_list = [vanilla, chocolate, cherry_ripple, lemon, butterscotch, licorice_riple],
    All_combinations = [
        {vanilla, vanilla}, {vanilla, chocolate}, {vanilla, cherry_ripple},
        {vanilla, lemon}, {vanilla, butterscotch}, {vanilla, licorice_riple},
        {chocolate, vanilla}, {chocolate, chocolate}, {chocolate, cherry_ripple},
        {chocolate, lemon}, {chocolate, butterscotch}, {chocolate, licorice_riple},
        {cherry_ripple, vanilla}, {cherry_ripple, chocolate}, {cherry_ripple, cherry_ripple},
        {cherry_ripple, lemon}, {cherry_ripple, butterscotch}, {cherry_ripple, licorice_riple},
        {lemon, vanilla}, {lemon, chocolate}, {lemon, cherry_ripple},
        {lemon, lemon}, {lemon, butterscotch}, {lemon, licorice_riple},
        {butterscotch, vanilla}, {butterscotch, chocolate}, {butterscotch, cherry_ripple},
        {butterscotch, lemon}, {butterscotch, butterscotch}, {butterscotch, licorice_riple},
        {licorice_riple, vanilla}, {licorice_riple, chocolate}, {licorice_riple, cherry_ripple},
        {licorice_riple, lemon}, {licorice_riple, butterscotch}, {licorice_riple, licorice_riple}
    ],
    [?_assertEqual(All_combinations, cone_combinations(Flavor_list)),
     ?_assertEqual([], cone_combinations([])),
     ?_assertEqual(fail, cone_combinations(nil))
    ].

most_popular_combinations_test_() ->
    All_combinations = [
        {vanilla, vanilla}, {vanilla, chocolate}, {vanilla, cherry_ripple},
        {vanilla, lemon}, {vanilla, butterscotch}, {vanilla, licorice_riple},
        {chocolate, vanilla}, {chocolate, chocolate}, {chocolate, cherry_ripple},
        {chocolate, lemon}, {chocolate, butterscotch}, {chocolate, licorice_riple},
        {cherry_ripple, vanilla}, {cherry_ripple, chocolate}, {cherry_ripple, cherry_ripple},
        {cherry_ripple, lemon}, {cherry_ripple, butterscotch}, {cherry_ripple, licorice_riple},
        {lemon, vanilla}, {lemon, chocolate}, {lemon, cherry_ripple},
        {lemon, lemon}, {lemon, butterscotch}, {lemon, licorice_riple},
        {butterscotch, vanilla}, {butterscotch, chocolate}, {butterscotch, cherry_ripple},
        {butterscotch, lemon}, {butterscotch, butterscotch}, {butterscotch, licorice_riple},
        {licorice_riple, vanilla}, {licorice_riple, chocolate}, {licorice_riple, cherry_ripple},
        {licorice_riple, lemon}, {licorice_riple, butterscotch}, {licorice_riple, licorice_riple}
    ],
    Sorted_combinations_with_purchases = lists:sort(lists:zip([rand:uniform(100000) || _ <- lists:seq(0, length(All_combinations)-1)], All_combinations)),
    {H, _Remainder} = lists:split(10, Sorted_combinations_with_purchases),
    [?_assertEqual(H, most_popular_combinations(10, Sorted_combinations_with_purchases)),
     ?_assertEqual(fail, most_popular_combinations(5, [])),
     ?_assertEqual(fail, most_popular_combinations(5, [{86656, {vanilla, vanilla}}])),
     ?_assertEqual(fail, most_popular_combinations(5, [{86656, {vanilla, vanilla}}, {58876, {vanilla, chocolate}}])),
     ?_assertEqual(fail, most_popular_combinations(5, [{86656, {vanilla, vanilla}},
                                                       {58876, {vanilla, chocolate}},
                                                       {23473, {vanilla, cherry_ripple}}])),
     ?_assertEqual(fail, most_popular_combinations(5, [{86656, {vanilla, vanilla}},
                                                       {58876, {vanilla, chocolate}},
                                                       {23473, {vanilla, cherry_ripple}},
                                                       {62009, {vanilla, lemon}}])),
     ?_assertEqual([], most_popular_combinations(0, Sorted_combinations_with_purchases)),
     ?_assertEqual(fail, most_popular_combinations(-3, Sorted_combinations_with_purchases))
    ].

-endif.