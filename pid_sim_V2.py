# %%
import numpy as np
from os import system as sys
import matplotlib.pyplot as plt
import matplotlib as mpl
from cycler import cycler
prop = mpl.font_manager.FontProperties(family='serif')
plt.rcParams["mathtext.fontset"] = "cm"
# mpl.rcParams['text.usetex'] = True
# mpl.rcParams['text.latex.preamble'] = [r'\usepackage(amsmath)']
p = plt.rcParams
p["figure.figsize"] = 6, 2.5
p["figure.facecolor"] = "#fff"
p["axes.axisbelow"] = True
p["axes.linewidth"] = 1
p["axes.facecolor"] = "#f9f9f9"
p["axes.ymargin"] = 0.04
p["axes.xmargin"] = 0.04

p["axes.grid"] = True
p["axes.grid.axis"] = "x"
p["grid.color"] = "#999999"
p["grid.linestyle"] = "--"

p["axes.spines.bottom"] = True
p["axes.spines.left"] = True
p["axes.spines.right"] = False
p["axes.spines.top"] = False

p["xtick.bottom"] = True
p["xtick.top"] = False
p["xtick.direction"] = "out"
p["xtick.major.size"] = 5
p["xtick.major.width"] = 1

p["ytick.left"] = True
p["ytick.right"] = False
p["ytick.direction"] = "out"
p["ytick.major.size"] = 5
p["ytick.major.width"] = 1

# p["lines.linewidth"] = 1
# # p["lines.marker"] = "o"
# # p["lines.markeredgewidth"] = 1.5
# p["lines.markeredgecolor"] = "auto"
# p["lines.markerfacecolor"] = "white"
# p["lines.markersize"] = 1
path_pid_diagram = "/home/mrrobot/Dropbox/2021B/JEAB_SNS_feedback/figure3.png"
img = mpl.image.imread(path_pid_diagram)
# %%

def axes_settings(fig, ax, title, xlab, ylab, leg_title, prop, xmult=10, ymult=0.5):
    ax.set_xlabel(xlab, fontsize=14)
    ax.set_ylabel(ylab)
    ax.legend(title=leg_title,
              loc='best',
              fontsize=6.5,
              prop=prop,
              framealpha=0.1,
              fancybox=False,
              facecolor="#F9F9F9",
              edgecolor="#F9F9F9",
              title_fontsize=10)
    ax.yaxis.set_major_locator(plt.MultipleLocator(ymult))
    ax.xaxis.set_major_locator(plt.MultipleLocator(xmult))
    # ax.set_title(title, x=0, pad=-15)
    ax.annotate(title.lower(),
                xy=(-20, 7 + ax.bbox.height),
                xycoords="axes pixels",fontsize=12, weight='bold') 
                # 


def integral(λ, int_vect, trial):
    length_int = np.arange(1, trial, 1)
    integrando = λ**(trial - length_int) * int_vect[length_int]
    return np.sum(integrando)


def pid_control(Kp, Ki, Kd, dt, Time, sp):
    n = int(round(Time / dt))  # number of samples
    Int_vect = np.zeros(n + 1)
    PID = np.zeros(n + 1)
    FeedBack = np.zeros(n + 1)
    Output = np.zeros(n + 1)
    Error = np.zeros(n + 1)
    STATE1 = np.zeros(n + 1)
    state2 = np.zeros(n + 1)
    STATE2 = np.zeros(n + 1)

    for i in range(n):
        Error[i+1] = sp[i] - FeedBack[i]
        Prop = Error[i+1]
        Der = (Error[i+1] - Error[i]) / dt
        Int_vect[i+1] = (Error[i+1] + Error[i]) * dt / 2
        I = integral(1, Int_vect[:(i+1)], i+1)
        PID[i+1] = Kp * Prop + Ki * I + Kd * Der

        STATE1[i+1] = np.sum(PID)
        state2[i+1] = (STATE1[i+1] + STATE1[i]) * dt / 2
        STATE2[i+1] = np.sum(state2)
        Output[i+1] = (STATE2[i+1] + STATE2[i]) * dt / 2
        FeedBack[i+1] = state2[i+1] + Output[i+1]
    return Output


def pid_inter_animal(Kp, Ki, Kd, dt, Time, sp, λ):
    n = int(round(Time / dt))
    Int_vect = np.zeros(n + 1)
    PID = np.zeros(n + 1)
    FeedBack = np.ones(n + 1)
    Output = np.zeros(n + 1)
    Output[0] = 1
    Error = np.zeros(n + 1)
    Error[0] = 1
    STATE1 = np.zeros(n + 1)
    state2 = np.zeros(n + 1)
    STATE2 = np.zeros(n + 1)
    disturbance = np.zeros(n + 1)

    for i in range(n):
        # fraction
        feed2 = np.random.uniform(0, 0.5, 1)
        # feed2 = np.random.normal(0.25, 0.3, 1)
        # disturbance is the distance of the robber, a fraction of sp
        disturbance[i] = sp[i] * feed2
        Error[i+1] = sp[i] - FeedBack[i] - disturbance[i]
        Prop = Error[i+1]
        Der = (Error[i+1] - Error[i]) / dt
        Int_vect[i+1] = (Error[i+1] + Error[i])*(dt/2)
        I = integral(λ, Int_vect[:(i+1)], i+1)
        PID[i+1] = Kp * Prop + Ki * I + Kd * Der
        STATE1[i+1] = np.sum(PID)
        state2[i+1] = (STATE1[i+1] + STATE1[i]) * dt / 2
        STATE2[i+1] = np.sum(state2)
        Output[i+1] = ((STATE2[i+1] + STATE2[i]) * dt/2)
        FeedBack[i+1] = state2[i+1] + Output[i+1]
        if Output[i+1] < -0.1:
            Output[(i+1):] = np.nan
            break
    print(np.mean(disturbance))
    return Output[0:-1], disturbance[0:-1]


# %% simulation
Kd = [0.01, 0.5, 1.5]
Ki = [0.1, 0.5, 1.5]
Kp = 0.9
dt = 0.01
Time = 30
n = int(round(Time / dt))  # number of samples
# set-point
sp = np.zeros(n)
sp[1:n] = 1
sp[int(n / 2):] = 2
T = np.arange(0, Time, dt)
# config. for plotting
mosaic = """
    AA
    BC
    DD
    """
fig = plt.figure(figsize=(7, 9))
axes = fig.subplot_mosaic(mosaic)

colores = [
    'orange', 'gray', 'red'
]
axes['A'].imshow(img)
axes_settings(fig,
              axes['A'],
              'A',
              xlab='',
              ylab='',
              leg_title='',
              prop=prop,
              xmult=15, ymult=1)
axes['A'].set_axis_off()

for ax in ['B', 'C']:
    print(ax)
    axes[ax].plot(
        T,
        sp,
        'b--',
        label=r'$r(t)$'
    )
    axes[ax].set_prop_cycle(cycler('color', colores) +
                            cycler('lw', [2, 2, 2]))
    for i in range(3):
        if ax == 'B':
            ki = 0.5
            out = pid_control(Kp, Kd[i], ki, dt, Time, sp)
            axes[ax].plot(
                T, out[1:],
                label=r'$k_D = {{{}}}$'.format(Kd[i])
            )
        else:
            kd = 0.5
            out = pid_control(Kp, kd, Ki[i], dt, Time, sp)
            axes[ax].plot(
                T, out[1:],
                label=r'$k_I = {{{}}}$'.format(Ki[i])
            )
        if ax == 'B':
            ylab = 'Response'
            # title = 'A'
            leg_title = r'$k_I = {{{}}}$'.format(ki)
        else:
            ylab = ''
            # title = 'B'
            leg_title = r'$k_D = {{{}}}$'.format(kd)
        axes_settings(fig,
                      axes[ax], ax, xlab=r'$t~(\mathrm{\sf time~units})$',
                      ylab=ylab,
                      leg_title=leg_title,
                      prop=prop,
                      xmult=15, ymult=1)

dt = 0.01
Time = 45
n = int(round(Time / dt))
T = np.arange(0, Time, dt)
sp = np.concatenate((np.ones(int(n/2)), np.ones(int(n/2))))
Kd = 0.5
Ki = 0.1

Kp = [0.1, 0.25, 0.75]

axes['D'].set_prop_cycle(cycler('color', ['r', 'k', 'orange']) +
                         cycler('lw', [2, 2, 2]))
for kp in Kp:
    out_dod, dist = pid_inter_animal(
        kp, Ki=Ki, Kd=Kd, dt=dt, Time=Time, sp=sp, λ=1)
    axes['D'].plot(
        T, out_dod,
        label=r'$k_P = {{{}}}$'.format(kp)
    )
    x = T[out_dod[~np.isnan(out_dod)].shape[0]-1]
    y = out_dod[~np.isnan(out_dod)][-1]
    axes['D'].scatter(
        x, y
    )
    if out_dod[~np.isnan(out_dod)].shape[0] < T.shape[0]:
        axes['D'].annotate(
            'robber succeeded',
            fontsize=10, family="serif",
            xy=(x, y), xycoords='data',
            xytext=(+x*1.1, +abs(y)*1.2), textcoords='data',
            arrowprops=dict(arrowstyle="->", connectionstyle="arc3, rad=-.2"))

axes_settings(fig,
              axes['D'],
              'D', xlab=r'$t~(\mathrm{\sf time~units})$',
              ylab='Response',
              leg_title='Proportional gain',
              prop=prop,
              xmult=15, ymult=0.5)

# fig.tight_layout()
# fig.tight_layout(pad=0, h_pad=-0.5)
fig.subplots_adjust(hspace=0.3)
fig.savefig("pid_sim.pdf", dpi=600, bbox_inches='tight')
# convert to png
convert = 'convert -density 600 -trim pid_sim.pdf -quality 100 pid_sim.png'
sys(convert)
print(
    "Expected disturbance = %0.3f" % np.mean(dist)
)
print(
    "(Set-point - Expected disturbance) = %0.3f" % (1-np.mean(dist))
)
# %%

# %%
