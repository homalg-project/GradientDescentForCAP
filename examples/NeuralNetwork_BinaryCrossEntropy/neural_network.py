import numpy as np
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Input, Dense
from tensorflow.keras.optimizers import Adam, SGD
from sklearn.model_selection import train_test_split

with open("data/training_examples.txt", "r") as f:
    f = f.read()

data = np.array(eval(f))

# create the data
X = np.array([example[:2] for example in data])
y = np.array([example[2] for example in data])

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

model = Sequential()

# Add an Input layer
model.add(Input(shape=(2,)))

# Add hidden layers
model.add(Dense(6, activation='relu'))
model.add(Dense(6, activation='relu'))
model.add(Dense(6, activation='relu'))

# Add output layer
model.add(Dense(1, activation='sigmoid'))

# Build the model with the input shape to initialize the weights
model.build()

# Summary of the model
model.summary()

# Specify the optimizer
#optimizer = Adam(learning_rate=0.001, beta_1=0.9, beta_2=0.999, epsilon=1e-07)
optimizer = SGD(learning_rate=0.01)

model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])

# View the initial weights
initial_weights = model.get_weights()

initial_weights_vec = []

for i in range(0, len(initial_weights), 2):
    w = initial_weights[i]
    b = initial_weights[i+1]
    b = b[np.newaxis, :]
    m = np.concatenate([w, b])
    print( f"affine matrix of layer {i//2 + 1}:\n{m}\n" )
    initial_weights_vec = m.flatten('F').tolist() + initial_weights_vec

print(f"initial weights as vector: \n{initial_weights_vec}")

# View the parameters of the Adam optimizer
print(f'Learning rate: {optimizer.learning_rate.numpy()}')
#print(f'Beta_1: {optimizer.beta_1}')
#print(f'Beta_2: {optimizer.beta_2}')
#print(f'Epsilon: {optimizer.epsilon}')

# Train the model
model.fit(X_train, y_train, epochs=50, batch_size=1)

# Evaluate the model. Accuracy should less or more than 83%
loss, accuracy = model.evaluate(X_test, y_test)
print(f'Accuracy: {accuracy}')

# View the learnt weights
weights = model.get_weights()

weights_vec = []
for i in range(0, len(weights), 2):
    w = weights[i]
    b = weights[i+1]
    b = b[np.newaxis, :]
    m = np.concatenate([w, b])
    print( f"affine matrix of layer {i//2 + 1}:\n{m}\n" )
    weights_vec = m.flatten('F').tolist() + weights_vec

print(f"weights as vector: \n{weights_vec}")
