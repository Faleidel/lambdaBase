lambdaBase
==========

The language is simple. There is 2 'thing' : lambda and literals.
Lambda are in the form :

> \x -> x y z ...

Lambda have only one argument. If you want a multi argument function you can nest lambdas like this :

> \x -> \y -> x + y

This mean partial application on lambda is possible.

In a programme like "add 123 321" lambdaBase cant execute the add function since it doesn't know what it is.
In this case, the toLit function of the Lit class is called.
In the presence of "add 123 321" the first 2 tokens ( add and 123 ) will be converted to member of the Lit class an reduced with the apply function.

To step to reducing "add 123 321" will be:

> add 123 321

> (toLit add) 123 321

> (litAdd) (toLit 123) 321

> (apply lit lit123) 321

> litAdd123 321

> litAdd123_321

> lit444

For an example of an emplementation of the Lit class, see the LambdaLit packaged.


By default lambdas are strict in it argument. If you want a lazy one ( to define an if function for example ) you can using a ~ like this :

> (\x ~> x) -- lazy
> (\x -> x) -- strict

An if function (lazy on everything but the condition) would look like this :

> (\if -> \True -> \False -> if True 123 321) (\c -> \b1 ~> \b2 ~> c b1 b2) (\b1 ~> \b2 ~> b1) (\b1 ~> \b2 ~> b2)
