<!-- BEGIN HEADER -->
# GradientDescentForCAP&ensp;<sup><sup>[![View code][code-img]][code-url]</sup></sup>

### Exploring categorical machine learning in CAP

| Documentation | Latest Release | Build Status | Code Coverage |
| ------------- | -------------- | ------------ | ------------- |
| [![HTML stable documentation][html-img]][html-url] [![PDF stable documentation][pdf-img]][pdf-url] | [![version][version-img]][version-url] [![date][date-img]][date-url] | [![Build Status][tests-img]][tests-url] | [![Code Coverage][codecov-img]][codecov-url] |

<!-- END HEADER -->
<!-- BEGIN FOOTER -->
---

### Dependencies

To obtain current versions of all dependencies, `git clone` (or `git pull` to update) the following repositories:

|    | Repository | git URL |
|--- | ---------- | ------- |
| 1. | [**homalg_project**](https://github.com/homalg-project/homalg_project#readme) | https://github.com/homalg-project/homalg_project.git |
| 2. | [**CAP_project**](https://github.com/homalg-project/CAP_project#readme) | https://github.com/homalg-project/CAP_project.git |
| 3. | [**CategoricalTowers**](https://github.com/homalg-project/CategoricalTowers#readme) | https://github.com/homalg-project/CategoricalTowers.git |

[html-img]: https://img.shields.io/badge/🔗%20HTML-stable-blue.svg
[html-url]: https://homalg-project.github.io/GradientDescentForCAP/doc/chap0_mj.html

[pdf-img]: https://img.shields.io/badge/🔗%20PDF-stable-blue.svg
[pdf-url]: https://homalg-project.github.io/GradientDescentForCAP/download_pdf.html

[version-img]: https://img.shields.io/endpoint?url=https://homalg-project.github.io/GradientDescentForCAP/badge_version.json&label=🔗%20version&color=yellow
[version-url]: https://homalg-project.github.io/GradientDescentForCAP/view_release.html

[date-img]: https://img.shields.io/endpoint?url=https://homalg-project.github.io/GradientDescentForCAP/badge_date.json&label=🔗%20released%20on&color=yellow
[date-url]: https://homalg-project.github.io/GradientDescentForCAP/view_release.html

[tests-img]: https://github.com/homalg-project/GradientDescentForCAP/actions/workflows/Tests.yml/badge.svg?branch=master
[tests-url]: https://github.com/homalg-project/GradientDescentForCAP/actions/workflows/Tests.yml?query=branch%3Amaster

[codecov-img]: https://codecov.io/gh/homalg-project/GradientDescentForCAP/branch/master/graph/badge.svg
[codecov-url]: https://app.codecov.io/gh/homalg-project/GradientDescentForCAP

[code-img]: https://img.shields.io/badge/-View%20code-blue?logo=github
[code-url]: https://github.com/homalg-project/GradientDescentForCAP#top
<!-- END FOOTER -->

### Running the Package with Docker
To save time on installation, you can use a pre-built Docker image to run the package:
```bash
docker run -it ghcr.io/kamalsaleh/gradient-descent-for-cap-docker:latest
```
This will install the computer algebra system Gap and all other dependencies in an isolated container environment.

### Introduction
This package is an implementation of the ideas presented in the paper [Deep Learning with Parametric Lenses](https://arxiv.org/abs/2404.00408) using the categorical programming language offered by the [CAP](https://github.com/homalg-project/CAP_project) project. The following is a brief overveiw of the operations offered by this repository.

Let $A$ and $B$ be two sets and $\Theta$ be a set of parameters. A parametrized map is a map of the form $f:\Theta\times A \to B$ where $A$ is the domain of the input variables, $\Theta$ the domain of parameters and $B$ the codomain. For each fixed $\theta \in \Theta$, we get the map $f_\theta:A \to B,~~a \mapsto f_\theta(a):=f(\theta,a)$.

In machine learning, both predictions maps and loss maps can be seen as parametrized maps, any they play distinct but complementary roles in the model training process. Let us break down how each of these fits into the concept of parametrized maps.

Let $\mathcal{D}$ be the **training set** containing $N$ _labeled examples_: $\mathcal{D}=$ { $(x^{[i]},y^{[i]})|i=1,2,\dots,N$ } $\subset X\times Y$.
- $x^{[i]}$ is the input-vector (feature vector) and it belongs to the input-space $X$.
- $y^{[i]}$ is the label-vector of $x^{[i]}$ and it belongs to the output-space $Y$.

A **prediction map** can be defined as a parametrized map $f:\Theta \times X \to Y$. For a given and parameters-vector $\theta\in\Theta$ and an input-vector $x\in X$, the prediction is $\hat{y}=f_\theta(x)\in Y$.

For example let $\mathcal{D}=$ { $(1,2.9), (2,5.1), (3,7.05)$ } $\subset \mathbb{R}^1\times \mathbb{R}^1 \simeq \mathbb{R}^2$. For a linear regression model, the prediction might be defined as follows:

$$f:\mathbb{R}^2\times \mathbb{R}^1 \to \mathbb{R}^1;~~~(\theta,x) \mapsto \theta_1 x + \theta_2$$

where $\theta=(\theta_1,\theta_2)\in \mathbb{R}^2$ is a parameter-vector. For instance, if $\theta=(2,1)$, then the prediction associated to the input $x=2$ is $\hat{y}=2\cdot 2 + 1=5$.

A **loss map** can be defined as a parametrized map $\ell:\Theta \times X \times Y \to \mathbb{R}$. It quantifies the discrepancy between the predicted output $\hat{y}=f_\theta(x) \in Y$ and the true output $y\in Y$. Given a training set, the **total loss** map is defined by

$$\mathcal{L}:\Theta \to \mathbb{R};~~~\theta \mapsto \frac{1}{N}\sum_{i=1}^{N}\ell(\theta,x^{[i]},y^{[i]})$$

where $N=|\mathcal{D}|$ (the number of labeled examples in the training set).

For a linear regression model, the loss map can be defined as follows:

$$\ell:\mathbb{R}^2\times \mathbb{R}^1 \times \mathbb{R}^1 \to \mathbb{R}, (\theta,x,y) \mapsto (\hat{y} - y)^2=(\theta_1 x + \theta_2 - y)^2$$

where $\theta=(\theta_1,\theta_2)\in \mathbb{R}^2$ is a parameter-vector. For instance, if $\theta=(2,1)$, then the total loss on the above training set $\mathcal{D}$ is $\frac{(2\cdot 1 + 1-2.9)^2+(2\cdot 2 + 1-5.1)^2+(2\cdot 3 + 1-7.05)^2}{3}=0.0074$.

Given a training set $\mathcal{D}\subset X\times Y$ and a loss map $\ell:\Theta \times X \times Y \to \mathbb{R}$, we would like to "learn" a parameter-vector $\theta\in\Theta$ such that the total loss $\mathcal{L}(\theta)$ is as small as possible. We illustrate the learning procedure via the next two examples.

## Example 1: Neural Network to Perform Linear-Regression

In this example, we consider a training dataset consisting of the three points  $\mathcal{D}=$ { $( 1, 2.9 ), ( 2, 5.1 ), ( 3, 7.05 )$ } $\subset\mathbb{R}^2$.

<div style="text-align: center;">
  <img src="pictures/training_data_1.png" alt="Local GIF" width="600" height="400"/>
</div>

We aim to compute a line that fits $\mathcal{D}$.
```julia
gap> LoadPackage( "GradientDescentForCAP" );
true

gap> Para := CategoryOfParametrisedMorphisms( SkeletalSmoothMaps );
CategoryOfParametrisedMorphisms( SkeletalSmoothMaps )

gap> D := [ [ 1, 2.9 ], [ 2, 5.1 ], [ 3, 7.05 ] ];;
```
Let us create a neural network with the following architecture:

<div style="text-align: center;">
  <img src="pictures/network-1.png" alt="Local GIF" width="400" height="200"/>
</div>

where the activation map applied on the output layer is the identity function _IdFunc_. Its input dimension is 1 and output dimension is 1 and has no hidden layers.

```julia
gap> input_dim := 1;; hidden_dims := [ ];; output_dim := 1;;

gap> f := PredictionMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "IdFunc" );;
```
As a parametrized map this neural network is defined as:

<img src="pictures/eq-1.png" alt="Image Description" width="1000" height="120">

Note that $(\theta_1,\theta_2)$ represents the parameters-vector while $(x)$ represents the input-vector. Hence, the above output is an affine transformation of $(x)\in \mathbb{R}^1$.
```julia
gap> input := ConvertToExpressions( [ "theta_1", "theta_2", "x" ] );
[ theta_1, theta_2, x ]

gap> Display( f : dummy_input := input );
ℝ^1 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^2

Underlying Morphism:
-------------------
ℝ^3 -> ℝ^1

‣ theta_1 * x + theta_2
```
Let us now evaluate this prediction map on a random parameters-vector in $\mathbb{R}^2$ and a random input-vector in $\mathbb{R}^1$.

```julia
gap> theta := [ 2, 1 ];; x := [ 2 ];;

gap> Eval( f, [ theta, x ] );
[ 5 ]
```

To train the neural network, we need to specify a loss map that will be used to learn the weights by minimizing the total loss. Since the activation map applied on the output layer is _IdFunc_, we use the _Quadratic-Loss_ map:

$$
\ell:\mathbb{R}^2\times \mathbb{R}^1 \times \mathbb{R}^1 \to \mathbb{R},~~ (\theta_1,\theta_2,x,y) \mapsto \text{Quadratic-Loss}\left( f((\theta_1, \theta_2,x)), (y) \right) = (\theta_1 x + \theta_2 - y)^2
$$

Note that $(\theta_1,\theta_2)$ represents the parameters-vector while $(x,y)$ represents an example in the training set. The loss map $\ell$ quantifies the discrepancy between the predicted vector $f((\theta_1, \theta_2,x)) \in \mathbb{R}^1$ and the true output $(y) \in \mathbb{R}^1$.

In the following we construct the aforementioned loss-map:

```julia
gap> ell := LossMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "IdFunc" );;

gap> input := ConvertToExpressions( [ "theta_1", "theta_2", "x", "y" ] );
[ theta_1, theta_2, x, y ]

gap> Display( ell : dummy_input := input );
ℝ^2 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^2

Underlying Morphism:
-------------------
ℝ^4 -> ℝ^1

‣ (theta_1 * x + theta_2 - y) ^ 2 / 1
```
In order to learn the parameters we need to specifiy an optimization procedure. In this example, we will use Gradient-Descent-Optimizer. Starting with initial values, it computes the gradient of the loss function and updates the parameters in the opposite direction of the gradient, scaled by a learning rate. This process continues until the loss function converges to a minimum.

```julia
gap> Lenses := CategoryOfLenses( SkeletalSmoothMaps );
CategoryOfLenses( SkeletalSmoothMaps )

gap> optimizer := Lenses.GradientDescentOptimizer( : learning_rate := 0.01 );;
```

Now we compute the One-Epoch-Update-Lens using the _batch size_ = 1:

```julia
gap> batch_size := 1;;

gap> one_epoch_update := OneEpochUpdateLens( ell, optimizer, D, batch_size );
(ℝ^2, ℝ^2) -> (ℝ^1, ℝ^0) defined by:

Get Morphism:
----------
ℝ^2 -> ℝ^1

Put Morphism:
----------
ℝ^2 -> ℝ^2
```
The _Get Morphism_ computes the total loss associated to a parameter-vector $\theta \in \mathbb{R}^2$ and _Put Morphism_ updates the parameter-vector.

Let us initialize a parameter-vector:

```julia
gap> theta := [ 0.1, -0.1 ];;
```

To perform _nr_epochs_ = 15 updates on $\theta\in\mathbb{R}^2$ we can use the _Fit_ operation:
```julia
gap> nr_epochs := 10;;

gap> theta := Fit( one_epoch_update, nr_epochs, theta );
Epoch  0/15 - loss = 26.777499999999993
Epoch  1/15 - loss = 13.002145872163197
Epoch  2/15 - loss = 6.3171942035316935
Epoch  3/15 - loss = 3.0722513061917534
Epoch  4/15 - loss = 1.4965356389126505
Epoch  5/15 - loss = 0.73097379078374358
Epoch  6/15 - loss = 0.35874171019291579
Epoch  7/15 - loss = 0.1775574969062125
Epoch  8/15 - loss = 0.089228700384937534
Epoch  9/15 - loss = 0.046072054531129378
Epoch 10/15 - loss = 0.024919378473509772
Epoch 11/15 - loss = 0.014504998499450883
Epoch 12/15 - loss = 0.0093448161379050161
Epoch 13/15 - loss = 0.0067649700132868147
Epoch 14/15 - loss = 0.0054588596501628835
Epoch 15/15 - loss = 0.0047859930295160499
[ 2.08995, 0.802632 ]
```

The parameter-vector after 15 epochs is $\theta=[2.08995, 0.802632]$. That is, the prediction map is $f_{\theta}:\mathbb{R}^1 \to \mathbb{R}^1,~~x \mapsto 2.08995x + 0.802632$.

```julia
gap> theta := SkeletalSmoothMaps.Constant( theta );
ℝ^0 -> ℝ^2

gap> f_theta := ReparametriseMorphism( f, theta );
ℝ^1 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^0

Underlying Morphism:
-------------------
ℝ^1 -> ℝ^1

gap> f_theta := UnderlyingMorphism( f_theta );;

gap> Display( f_theta );
ℝ^1 -> ℝ^1

‣ 2.08995 * x1 + 0.802632
```

Let us compute the predicted values associated to $x\in$ {1,2,3}.
```julia
gap> Eval( f_theta, [ 1 ] );
[ 2.89259 ]

gap> Eval( f_theta, [ 2 ] );
[ 4.98254 ]

gap> Eval( f_theta, [ 3 ] );
[ 7.07249 ]
```

The following image illustrates the lines defined by the parameter-vectors over the course of the 15 epochs.
<div style="text-align: center;">
  <img src="pictures/linear_regression.gif" alt="Local GIF" width="600" height="400"/>
</div>

## Example 2: Neural Network to Perform Multi-Class Classification
In this example, we consider a training dataset consisting of points in the two-dimensional Euclidean space, $\mathbb{R}^2$. Each point in this space is represented by a pair of real numbers (coordinates). The goal is to classify these points into one of three distinct categories.

<div style="text-align: center;">
  <img src="pictures/training_data_2.png" alt="Local GIF" width="600" height="400"/>
</div>

To facilitate this classification, we use a one-hot encoding scheme for the labels. There are three possible classes for the data points. We denote these classes as _class-1_ (red), _class-2_ (green), and _class-3_ (blue). The labels are encoded using one-hot vectors. A one-hot vector is a binary vector with a length equal to the number of classes, where only the element corresponding to the true class is 1, and all other elements are 0. That is
  - _class-1_ : [1, 0, 0]
  - _class-2_ : [0, 1, 0]
  - _class-3_ : [0, 0, 1]

That is, the training set (the set of labeled exmaples) is a finite subset of $\mathbb{R}^2 \times \mathbb{R}^3 \simeq \mathbb{R}^5$.

```julia
gap> LoadPackage( "GradientDescentForCAP" );
true

gap> Para := CategoryOfParametrisedMorphisms( SkeletalSmoothMaps );
CategoryOfParametrisedMorphisms( SkeletalSmoothMaps )

gap> class_1 := Concatenation( List( [ -2 .. 3 ], i -> [ [ i, i - 1, 1, 0, 0 ], [ i + 1, i - 1, 1, 0, 0 ] ] ) );;

gap> class_2 := Concatenation( List( [ -3 .. -1 ], i -> List( [ i + 1 .. - i - 1 ], j -> [ i, j, 0, 1, 0 ] ) ) );;

gap> class_3 := Concatenation( List( [ 1 .. 3 ], i -> List( [ - i + 1 .. i - 1 ], j -> [ j, i, 0, 0, 1 ] ) ) );;

gap> D := Concatenation( class_1, class_2, class_3 );
[ [ -2, -3, 1, 0, 0 ], [ -1, -3, 1, 0, 0 ], [ -1, -2, 1, 0, 0 ], [ 0, -2, 1, 0, 0 ], [ 0, -1, 1, 0, 0 ],
  [ 1, -1, 1, 0, 0 ], [ 1, 0, 1, 0, 0 ], [ 2, 0, 1, 0, 0 ], [ 2, 1, 1, 0, 0 ], [ 3, 1, 1, 0, 0 ],
  [ 3, 2, 1, 0, 0 ], [ 4, 2, 1, 0, 0 ], [ -3, -2, 0, 1, 0 ], [ -3, -1, 0, 1, 0 ], [ -3, 0, 0, 1, 0 ],
  [ -3, 1, 0, 1, 0 ], [ -3, 2, 0, 1, 0 ], [ -2, -1, 0, 1, 0 ], [ -2, 0, 0, 1, 0 ], [ -2, 1, 0, 1, 0 ],
  [ -1, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 1 ], [ -1, 2, 0, 0, 1 ], [ 0, 2, 0, 0, 1 ], [ 1, 2, 0, 0, 1 ],
  [ -2, 3, 0, 0, 1 ], [ -1, 3, 0, 0, 1 ], [ 0, 3, 0, 0, 1 ], [ 1, 3, 0, 0, 1 ], [ 2, 3, 0, 0, 1 ] ]

```

Let us create a neural network with the following architecture:
<div style="text-align: center;">
  <img src="pictures/network-2.png" alt="Local GIF" width="400" height="200"/>
</div>

where the activation map applied on the output layer is _Softmax_.

Its input dimension is 2 and output dimension is 3 and has no hidden layers.
```julia
gap> input_dim := 2;; hidden_dims := [ ];; output_dim := 3;;

gap> f := PredictionMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "Softmax" );;
```

As a parametrized map this neural network is defined as:

<img src="pictures/eq-2.png" alt="Image Description" width="1000" height="120">

Note that $(\theta_1,\dots,\theta_9)$ represents the parameters-vector while $(x_{1},x_{2})$ represents the input-vector. Hence, the above output is the _Softmax_ of an affine transformation of $(x_{1},x_{2})$.
```julia
gap> input := ConvertToExpressions( [ "theta_1", "theta_2", "theta_3", "theta_4", "theta_5", "theta_6", "theta_7", "theta_8", "theta_9", "x1", "x2" ] );
[ theta_1, theta_2, theta_3, theta_4, theta_5, theta_6, theta_7, theta_8, theta_9, x1, x2 ]

gap> Display( f : dummy_input := input );
ℝ^2 -> ℝ^3 defined by:

Underlying Object:
-----------------
ℝ^9

Underlying Morphism:
-------------------
ℝ^11 -> ℝ^3

‣ Exp( (theta_1 * x1 + theta_2 * x2 + theta_3) )
    / (Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ))
‣ Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) )
    / (Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ))
‣ Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) )
    / (Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ))
```
Let us now evaluate this prediction map on a random parameters-vector in $\mathbb{R}^9$ and a random input-vector in $\mathbb{R}^2$.

```julia
gap> theta := [ 0.1, -0.1, 0, 0.1, 0.2, 0, -0.2, 0.3, 0 ];;
gap> input_vec := [ 1, 2 ];;

gap> prediction_vec := Eval( f, [ theta, input_vec ] );
[ 0.223672, 0.407556, 0.368772 ]

gap> PositionMaximum( prediction_vec );
2
```
That is, the input-vector $x=[1, 2]$ is predicted to belong to _class-2_ (which is of course incorrect as the neural network has not been yet trained).

To train the neural network, we need to specify a loss map that will be used to learn the weights by minimizing the total loss. Since the activation map applied on the output layer is _Softmax_, we use the _Cross-Entropy_ loss map:

$$
\ell:\mathbb{R}^9\times \mathbb{R}^2 \times \mathbb{R}^3 \to \mathbb{R},~~ (x_1,\dots,x_{14}) \mapsto \text{Cross-Entropy}\left( f((\theta_1, \dots, \theta_9,x_{1},x_{2})), (y_{1}, y_{2}, y_{3}) \right)
$$

Note that $(\theta_1,\dots,\theta_9)$ represents the parameters-vector while $(x_{1},x_{2},y_{1},y_{2},y_{3})$ represents an example in the training set. The loss map $\ell$ quantifies the discrepancy between the predicted probabilities vector $f((\theta_1, \dots, \theta_9,x_{1},x_{2})) \in \mathbb{R}^3$ and the true label $(y_{1}, y_{2}, y_{3}) \in$ { $(1,0,0),(0,1,0),(0,0,1)$ } $\in \mathbb{R}^3$.

More explicitely, if $(z_1,z_2,z_3) := f((\theta_1, \dots, \theta_9,x_{1},x_{2})) \in \mathbb{R}^3$, then

$$\text{Cross-Entropy}((z_1,z_2,z_3),(y_{1},y_{2},y_{3})) := -\frac{1}{3}\left(y_1\log(z_1)+y_2\log(z_2)+y_3\log(z_3)\right)$$

In the following we construct the aforementioned loss-map:

```julia
gap> ell := LossMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "Softmax" );;

gap> input := ConvertToExpressions( [ "theta_1", "theta_2", "theta_3", "theta_4", "theta_5", "theta_6", "theta_7", "theta_8", "theta_9", "x1", "x2", "y1", "y2", "y3" ] );

gap> Display( ell : dummy_input := input );
ℝ^5 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^9

Underlying Morphism:
-------------------
ℝ^14 -> ℝ^1

‣ ((Log( Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ) ) - (theta_1 * x1 + theta_2 * x2 + theta_3)) * y1
  + (Log( Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ) ) - (theta_4 * x1 + theta_5 * x2 + theta_6)) * y2
  + (Log( Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ) ) - (theta_7 * x1 + theta_8 * x2 + theta_9)) * y3) / 3
```
Until now, we have the training set $\mathcal{D} \subseteq \mathbb{R}^2\times \mathbb{R}^3 \simeq \mathbb{R}^5$ and a loss-map $\ell:\mathbb{R}^9\times \mathbb{R}^2 \to \mathbb{R}$. In order to learn the parameters we need to specifiy an optimization procedure.

In this example, we want to use the Adam-Optimizer procedure. The Adam-Optimizer is an extension of stochastic gradient descent that computes adaptive learning rates for each parameter. When using the Adam-Optimizer, additional auxiliary weights are maintained for each parameter to store moment estimates. Specifically, for a model with $n$ parameters, the Adam-Optimizer maintains:
  
  - Time Step ($\mathbf{t}$): one auxiliary weight.
  - First Moment Estimates ($\mathbf{m}$): $n$ auxiliary weights.
  - Second Moment Estimates ($\mathbf{v}$): $n$ auxiliary weights.
  - Original Model Weights: $n$ parameters.

So, in total, the Adam-Optimizer uses $1+3n$ weights. Since our network has $9$ parameters, the Adam-Optimizer uses $1+3\cdot 9=28$ weights: the first $1+2\cdot 9=19$ weights are auxiliary weights and the last $9$ weights are the original model weights.

```julia
gap> Lenses := CategoryOfLenses( SkeletalSmoothMaps );
CategoryOfLenses( SkeletalSmoothMaps )

gap> optimizer := Lenses.AdamOptimizer( : learning_rate := 0.01, beta_1 := 0.9, beta_2 := 0.999 );;

gap> optimizer( 9 )
(ℝ^28, ℝ^28) -> (ℝ^9, ℝ^9) defined by:

Get Morphism:
----------
ℝ^28 -> ℝ^9

Put Morphism:
----------
ℝ^37 -> ℝ^28
```

Now we compute the One-Epoch-Update-Lens using the _batch size_ = 1:

```julia
gap> batch_size := 1;;

gap> one_epoch_update := OneEpochUpdateLens( ell, optimizer, D, batch_size );;
(ℝ^28, ℝ^28) -> (ℝ^1, ℝ^0) defined by:

Get Morphism:
----------
ℝ^28 -> ℝ^1

Put Morphism:
----------
ℝ^28 -> ℝ^28
```
The _Get Morphism_ computes the total loss associated to the extended parameter-vector and _Put Morphism_ updates the extended parameter-vector.


Let us initialize a parameter-vector:

```julia
gap> t := [ 1 ];; # one entry
gap> m := [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]; # 9 entries
gap> v := [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]; # 9 entries
gap> theta := [ 0.1, -0.1, 0, 0.1, 0.2, 0, -0.2, 0.3, 0 ];; # 9 entries

gap> w := Concatenation( t, m, v, theta );;
```

The total loss associated to the extended parameter-vector $w$ we can use the "Get Morphism":

```julia
gap> current_loss := Eval( GetMorphism( one_epoch_update ), w );
[ 0.345836 ]
```

And to update the extended parameter-vector (one-epoch update) we use the "Put Morphism":

```julia
gap> new_w := Eval( PutMorphism( one_epoch_update ), w );
[ 31, 0.104642, -0.355463, -0.197135, -0.109428, -0.147082, 0.00992963, 
   0.00478679, 0.502546, 0.187206, 0.0105493, 0.00642903, 0.00211548, 
   0.00660062, 0.00274907, 0.00110985, 0.00278786, 0.0065483, 0.00112838, 
   5.45195, -1.26208, 3.82578, -5.40639, -0.952146, -3.42835, -2.79496, 3.09008, -6.80672 ]
```

To perform _nr_epochs_ = 4 updates on $w$ we can use the _Fit_ operation:

```julia
gap> nr_epochs := 4;;

gap> w := Fit( one_epoch_update, nr_epochs, w );
Epoch 0/4 - loss = 0.34583622811001763
Epoch 1/4 - loss = 0.6449437167091393
Epoch 2/4 - loss = 0.023811108587716449
Epoch 3/4 - loss = 0.0036371652708073405
Epoch 4/4 - loss = 0.0030655216725219204
[ 121, -4.57215e-05, -0.00190417, -0.0014116, -0.00181528, 0.00108949, 0.00065518, 0.001861, 0.000814679,
  0.000756424, 0.0104885, 0.00846858, 0.0022682, 0.00784643, 0.00551702, 0.0014626, 0.00351408, 0.00640225,
  0.00115053, 5.09137, -4.83379, 3.06257, -5.70976, 0.837175, -4.23622, -1.71171, 5.54301, -4.80856 ]
```

Now let us use the updated theta (is the last $9$ entries) to predict the label $\in$ {_class-1_, _class-2_, _class-3_} of the point $[1,-1]\in\mathbb{R}^2$.

```julia
gap> theta := SplitDenseList( w, [ 19, 9 ] )[2];
[ 5.09137, -4.83379, 3.06257, -5.70976, 0.837175, -4.23622, -1.71171, 5.54301, -4.80856 ]

gap> theta := SkeletalSmoothMaps.Constant( theta );
ℝ^0 -> ℝ^9

gap> f_theta := ReparametriseMorphism( f, theta );
ℝ^2 -> ℝ^3 defined by:

Underlying Object:
-----------------
ℝ^0

Underlying Morphism:
-------------------
ℝ^2 -> ℝ^3

gap> f_theta := UnderlyingMorphism( f_theta );;

gap> Display( f_theta );
ℝ^2 -> ℝ^3

‣ Exp( (5.09137 * x1 + (- 4.83379) * x2 + 3.06257) ) /
    (Exp( 5.09137 * x1 + (- 4.83379) * x2 + 3.06257 ) + Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) ) + Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ))
‣ Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) ) /
    (Exp( 5.09137 * x1 + (- 4.83379) * x2 + 3.06257 ) + Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) ) + Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ))
‣ Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ) /
    (Exp( 5.09137 * x1 + (- 4.83379) * x2 + 3.06257 ) + Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) ) + Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ))
    
gap> input_vec := [ 1, -1 ];;

gap> predictions_vec := Eval( f_theta, input_vec );
[ 1., 4.74723e-11, 1.31974e-11 ]

gap> PositionMaximum( predictions_vec );
1
```

That is, the predicted label of the input-vector $[1, -1]$ is _class-1_ which is indeed correct.

```julia
gap> predictions_vec := Eval( f_theta, [ 1, 3 ] );
[ 7.13122e-08, 2.40484e-08, 1. ]

gap> PositionMaximum( predictions_vec );
3
```

That is, the predicted label of the input-vector $[1, 3]$ is _class-3_ which is indeed correct.

The following image illustrates the predictions of the points {$(0.5i,0.5j)|i,j\in${$-6,-5,\dots,5,6$}} over the course of the $4$ epochs.

<div style="text-align: center;">
  <img src="pictures/logistic_regression.gif" alt="Local GIF" width="600" height="400"/>
</div>
