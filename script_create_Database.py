### Boa parte do dataset será baseado no artigo do MIC: Detecting Novel Associations in Large Data Sets 
## Mas outras funções serão adicionadas também

import numpy as np
import pandas as pd

def gerar_dados(func, x_min, x_max, n_pontos=1000, num=1000):
    dados = []

    for i in range(num):
        x = np.random.uniform(x_min, x_max, n_pontos)
        y = func(x)
        noises = [0.3, 0.65, 1.0]
        y_noise = []

        for noise in noises:
            y_noise.append(y + np.random.normal(0, noise, size=n_pontos))

        y_noisy = {}
        for noise in noises:
            y_noisy[f"y_noise_{noise}"] = y + np.random.normal(0, noise*np.std(y), size=n_pontos)

        df_temp = pd.DataFrame({
            'x': x,
            'y': y,
            'simulacao': i,
            **y_noisy
        })

        dados.append(df_temp)

    df_final = pd.concat(dados, ignore_index=True)    
    return df_final

def Linear(x):
    y = x
    return y

def Parabolic(x):
    y = 4*(x**2)
    return y

def Cubic(x):
    y = 128*((x-(1/3))**3) - 48*((x - (1/3))**2) - 12*(x - (1/3)) + 2
    return y

def Exponential(x):
    y = 10**(3*x) - 1
    return y

def Linear_Periodic(x):
    y = np.sin(10*np.pi*x) + x
    return y

def Sinusoidal_Fourier_Frequency(x):
    y = np.sin(16*np.pi*x)
    return y 

def Sinusoidal_non_Fourier_Frequency(x):
    y = np.sin(13*np.pi*x)
    return y 

def Sinusoidal_Varying_Frequency(x):
    y = np.sin(7*np.pi*x*(1 + x))
    return y 

def Random(x):
    return np.random.random(size=len(x))

def W_shaped(x):
    return np.where(x < 0, np.abs(x + 0.5), np.abs(x - 0.5))

def Circular(x):
    z = np.random.choice([-1, 1], size=len(x))
    return z * np.sqrt(1 - x**2)

def Logarithmic(x):
    return np.log(x + 1e-5)

def Step(x):
    return (x > 0.5).astype(float)

def Piecewise(x):
    return np.where(x < 0.5, x, 1 - x)

def Heteroscedastic(x):
    return x + np.random.normal(0, np.abs(x), size=len(x))

def X_shape(x):
    z = np.random.choice([-1, 1], size=len(x))
    return z * x

def Diamond(x):
    z = np.random.choice([-1, 1], size=len(x))
    return z * (1 - np.abs(x))

def TwoParabolas(x):
    z = np.random.choice([-1, 1], size=len(x))
    return z * (x**2)

def LocalDependence(x):
    y = np.random.normal(0, 0.1, size=len(x))
    mask = (x > 0.4) & (x < 0.6)
    y[mask] = x[mask]
    return y

def Radial(x):
    return np.exp(-((x - 0.5)**2) / 0.02)

def LogisticMap(x):
    r = 3.9
    return r * x * (1 - x)

def Sigmoid(x):
    x = np.asarray(x)
    y = np.zeros_like(x, dtype=float)
    mask1 = x <= 0.49
    mask2 = (x > 0.49) & (x <= 0.51)
    mask3 = x > 0.51
    y[mask1] = 0
    y[mask2] = 50*(x[mask2] - 0.5) + 0.5
    y[mask3] = 1
    return y

def Spike(x):
    x = np.asarray(x)
    y = np.zeros_like(x, dtype=float)
    mask1 = x < 1/20
    mask2 = (x >= 1/20) & (x < 1/10)
    mask3 = x >= 1/10
    y[mask1] = 20 * x[mask1]
    y[mask2] = -18 * x[mask2] + 19/10
    y[mask3] = -x[mask3]/9 + 1/9
    return y

def Spiral(x):
    t = 4*np.pi*x
    return t * np.sin(t)

def Bimodal(x):
    z = np.random.choice([0, 1], size=len(x))
    return z * x + (1 - z) * (1 - x)

def Sawtooth(x):
    return x - np.floor(x)

df_Linear = gerar_dados(Linear, -1, 1)
df_Parabolic = gerar_dados(Parabolic, -1, 1)
df_Cubic = gerar_dados(Cubic, -1.5, 2.5)
df_Exponential = gerar_dados(Exponential, -0.25, 1.25)
df_Linear_Periodic = gerar_dados(Linear_Periodic, -1, 1)
df_Sinusoidal_Fourier_Frequency = gerar_dados(Sinusoidal_Fourier_Frequency, -1, 1)
df_Sinusoidal_non_Fourier_Frequency = gerar_dados(Sinusoidal_non_Fourier_Frequency, -1, 1)
df_Sinusoidal_Varying_Frequency = gerar_dados(Sinusoidal_Varying_Frequency, -1, 1)
df_Random = gerar_dados(Random, -1, 1)
df_W_shaped = gerar_dados(W_shaped, -1, 1)
df_Circular = gerar_dados(Circular, -1, 1)
df_Logarithmic = gerar_dados(Logarithmic, 0, 1)
df_Step = gerar_dados(Step, -0.5, 1.5)
df_Piecewise = gerar_dados(Piecewise, -10, 10)
df_Heteroscedastic = gerar_dados(Heteroscedastic, -10, 10)
df_X_shape = gerar_dados(X_shape, -1, 1)
df_Diamond = gerar_dados(Diamond, -1, 1)
df_TwoParabolas = gerar_dados(TwoParabolas, -1, 1)
df_LocalDependence = gerar_dados(LocalDependence, -1, 1)
df_Radial = gerar_dados(Radial, -1, 1)
df_LogisticMap = gerar_dados(LogisticMap, -1, 1)
df_Sigmoid = gerar_dados(Sigmoid, 0, 1)
df_Spike = gerar_dados(Spike, 0, 1)
df_Spiral = gerar_dados(Spiral, -10, 10)
df_Bimodal = gerar_dados(Bimodal, -10, 10)    
df_Sawtooth = gerar_dados(Sawtooth, -10, 10)


with pd.ExcelWriter("dados_funcoes.xlsx", engine="openpyxl") as writer:
    df_Linear.to_excel(writer, sheet_name="Linear", index=False)
    df_Parabolic.to_excel(writer, sheet_name="Parabolic", index=False)
    df_Cubic.to_excel(writer, sheet_name="Cubic", index=False)
    df_Exponential.to_excel(writer, sheet_name="Exponential", index=False)
    df_Linear_Periodic.to_excel(writer, sheet_name="Linear-Periodic", index=False)
    df_Sinusoidal_Fourier_Frequency.to_excel(writer, sheet_name="Sinusoidal(Fourier Frequency)", index=False)
    df_Sinusoidal_non_Fourier_Frequency.to_excel(writer, sheet_name="Sinusoidal (non-Fourier Frequency)", index=False)
    df_Sinusoidal_Varying_Frequency.to_excel(writer, sheet_name="Sinusoidal (Varying Frequency)", index=False)
    df_Random.to_excel(writer, sheet_name="Random", index=False)
    df_W_shaped.to_excel(writer, sheet_name="W shaped", index=False)
    df_Circular.to_excel(writer, sheet_name="Circular", index=False)
    df_Logarithmic.to_excel(writer, sheet_name="Logarithmic", index=False)
    df_Step.to_excel(writer, sheet_name="Step", index=False)
    df_Piecewise.to_excel(writer, sheet_name="Piecewise", index=False)
    df_Heteroscedastic.to_excel(writer, sheet_name="Heteroscedastic", index=False)
    df_X_shape.to_excel(writer, sheet_name="X shape", index=False)
    df_Diamond.to_excel(writer, sheet_name="Diamond", index=False)
    df_TwoParabolas.to_excel(writer, sheet_name="Two Parabolas", index=False)
    df_LocalDependence.to_excel(writer, sheet_name="Local Dependence", index=False)
    df_Radial.to_excel(writer, sheet_name="Radial", index=False)
    df_LogisticMap.to_excel(writer, sheet_name="LogisticMap", index=False)
    df_Sigmoid.to_excel(writer, sheet_name="Sigmoid", index=False)
    df_Spike.to_excel(writer, sheet_name="Spike", index=False)
    df_Spiral.to_excel(writer, sheet_name="Spiral", index=False)
    df_Bimodal.to_excel(writer, sheet_name="Bimodal", index=False)
    df_Sawtooth.to_excel(writer, sheet_name="Sawtooth", index=False)