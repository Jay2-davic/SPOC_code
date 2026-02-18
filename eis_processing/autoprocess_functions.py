# autoprocess_functions.py
#%%
# loads the relevant EIS processing packages
from impedance import preprocessing as proc
from impedance import visualization as viz
from impedance.validation import linKK
from impedance import models
from impedance.models import circuits as mods
from galvani import BioLogic
from impedance.models.circuits import Randles, CustomCircuit
from scipy.optimize import minimize

# loads warning handler
import warnings
warnings.filterwarnings("ignore")

# loads file handling, data processing and visualization tools
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import io
import sys
import time
from contextlib import redirect_stdout, contextmanager
import textwrap

# loads export tools
import csv

# loads math tools
import math

# suppress output calculations
pd.options.mode.chained_assignment = None  # default='warn'

@contextmanager
def suppress_stdout(suppress=True):
    if suppress:
        sys.stdout = open(os.devnull, 'w')
    try:
        yield
    finally:
        sys.stdout = sys.__stdout__

HEADLESS = os.environ.get("EIS_HEADLESS", "1") == "1"

# -------------------------
# Optional progress emitter, set in worker by autoprocess_EIS
# -------------------------
PROGRESS_EMITTER = None

def emit_progress(event: str, **kw):
    """
    Emit a progress event to the parent process if a PROGRESS_EMITTER is set.
    The parent will render per worker bars with attempt counts.
    """
    global PROGRESS_EMITTER
    if PROGRESS_EMITTER is None:
        return
    try:
        PROGRESS_EMITTER(event=event, **kw)
    except Exception:
        pass

# -------------------------
# Cleaning functions
# -------------------------

def MPR_convert(EIS_file):
    warnings.simplefilter(action='ignore', category=FutureWarning)
    fileConversion = BioLogic.MPRfile(EIS_file)
    MPRtoDF = pd.DataFrame(fileConversion.data)

    MPRtoDF.columns = MPRtoDF.columns.str.split('/').str[0].str.strip()
    MPRtoDF.columns = MPRtoDF.columns.str.replace(' ', '_')
    MPRtoDF.columns = MPRtoDF.columns.str.replace('|', '')
    MPRtoDF.columns = MPRtoDF.columns.str.replace('-', '')
    MPRtoDF.columns = MPRtoDF.columns.str.replace('<', '')
    MPRtoDF.columns = MPRtoDF.columns.str.replace('>', '_meas')

    MPRtoDF = MPRtoDF.sort_values(by='time', ascending=True)
    MPRtoDF = MPRtoDF.reset_index()
    return MPRtoDF


def EIS_CycleLabels(EIS_file):
    try:
        df = MPR_convert(EIS_file)
    except Exception:
        return None

    max_freq = df['freq'].max()
    counter = 1
    cycle = []
    for i in df['freq']:
        if i == max_freq:
            cycle.append(counter)
            counter += 1
        else:
            cycle.append(None)

    df['cycle'] = cycle
    df['cycle'] = df['cycle'].fillna(method='ffill')
    return df


def EIS_CycleSplitter(EIS_file, output_path):
    df = EIS_CycleLabels(EIS_file)
    filename = os.path.basename(EIS_file).replace('.mpr', f'_RUN_')
    filename = fr'{output_path}\{filename}'

    try:
        for i, g in df.groupby('cycle'):
            df = df.sort_values('cycle')
            if not os.path.exists(filename):
                g.to_csv(f'{filename}{{:.0f}}.csv'.format(i), header=True, index_label=True)
            else:
                return None
    except Exception:
        return None

    return filename


def EIS_conversion(data_dictionary, output_path):
    database_csv = fr'{data_dictionary}'
    df = pd.read_csv(database_csv).dropna(subset=['filepath'])
    print(df['filepath'])

    list(map(lambda x: EIS_CycleSplitter(x, output_path), df['filepath']))


def ApplyFits(EIS_file):
    if ".mpr" in EIS_file:
        df = MPR_convert(EIS_file)
    elif ".csv" in EIS_file:
        df = pd.read_csv(EIS_file)
    else:
        raise ValueError('Files must be .mpr or .csv type')

    for col in df.columns:
        if 'Re' in col and 'Z' in col:
            df = df.rename(columns={col: 'Re'})
        elif 'Im' in col and 'Z' in col:
            df = df.rename(columns={col: 'Im'})
        elif col == '#NAME?':
            df = df.rename(columns={col: 'Im'})

    try:
        re_col = df['Re(Z)']
    except KeyError:
        re_col = df['Re']

    try:
        im_col = df['Im(Z)']
    except KeyError:
        im_col = df['Im']

    Z = pd.array(re_col - im_col * 1j)
    frequencies = pd.array(df.freq)

    frequencies, Z = proc.ignoreBelowX(frequencies, Z)
    return Z, frequencies


def printToDF(text_string):
    output_stream = io.StringIO()
    with redirect_stdout(output_stream):
        print(text_string)
    captured_lines = output_stream.getvalue().strip().split('\n')
    df = pd.DataFrame({'Printed Content': captured_lines})
    return df


def EIS_InitialCleaner(df):
    df.columns = ['column']
    df['A'] = df['column'].str.split(' = | \[|\]', expand=False)
    df = pd.DataFrame(df['A'].tolist()).fillna('')
    df = np.transpose(df)
    df = df.rename(columns=df.iloc[0], inplace=False).drop([0, 2, 3])
    df = df.rename(columns=lambda x: x.strip(), inplace=False)
    return df


def EIS_HeaderCleaner(df):
    df.columns = ['column']
    df['A'] = df['column'].str.split(': ', expand=False)
    df = pd.DataFrame(df['A'].tolist()).fillna('')
    df = np.transpose(df)
    df[1] = df[1].str.replace(' string', '')
    df = df.rename(columns=df.iloc[0], inplace=False).drop([0])
    df = df.rename(columns=lambda x: x.strip(), inplace=False)
    return df


def EIS_FittedCleaner(df):
    df.columns = ['column']
    df['A'] = df['column'].str.split(' = | \[|\]|\(+', expand=False)
    df = pd.DataFrame(df['A'].tolist()).fillna('')

    df[2] = df[2].str.replace('\+/\- |\)', '')
    df[1] = df[1].str.replace(' ', '')
    df[0] = df[0].str.replace(' ', '')

    df_act = df[[0, 1]]
    df_act = np.transpose(df_act)
    df_act = df_act.rename(columns=df_act.iloc[0]).drop([0])
    df_act = df_act.rename(columns=lambda x: x.strip(), inplace=False)

    df_err = df[[0, 2]]
    df_err = np.transpose(df_err)
    df_err = df_err.rename(columns=df_err.iloc[0]).drop([0])
    df_err = df_err.add_suffix('_stDev')

    combined = pd.concat([df_act.reset_index(drop=True), df_err.reset_index(drop=True)], axis=1)
    return combined


def EISresult_cleaner(text_string):
    df = printToDF(text_string)
    df_headerInfo = df.loc[0:2]
    df_initial = df.loc[5:9]
    df_fitted = df.loc[12:21]

    df_initial_ = EIS_InitialCleaner(df_initial).add_prefix('InitialGuess.')
    df_fitted_ = EIS_FittedCleaner(df_fitted).add_prefix('FittedValue.')
    df_headerInfo_ = EIS_HeaderCleaner(df_headerInfo).add_prefix('EISmeasurement.')

    df_combined = pd.concat([df_initial_.reset_index(drop=True), df_fitted_.reset_index(drop=True)], axis=1)
    df_combined = pd.concat([df_combined, df_headerInfo_.reset_index(drop=True)], axis=1)
    return df_combined


# -------------------------
# Analysis functions
# -------------------------

def calcValues(Z_actual, Z_predict):
    resid = np.array(np.log10(Z_actual) - np.log10(Z_predict))
    Z_real = resid.real / np.abs(np.log10(Z_actual))
    Z_imag = resid.imag / np.abs(np.log10(Z_actual))

    mse_real = np.square(Z_real).mean()
    rmse_real = math.sqrt(mse_real)

    mse_imag = np.square(Z_imag).mean()
    rmse_imag = math.sqrt(mse_imag)

    return rmse_real, rmse_imag, Z_real, Z_imag


def EIS_validate(file_name):
    Z, frequencies = ApplyFits(file_name)
    Z = Z.to_numpy()

    M, mu, Z_linKK, res_real, res_imag = linKK(frequencies, Z, c=.5, max_M=100, fit_type='complex', add_cap=True)
    mu = round(mu, 2)
    df = pd.DataFrame({'res_real': res_real, 'res_imag': res_imag})

    kk_real = sum((df['res_real'] ** 2) / len(df)) ** (1 / 2)
    kk_imag = sum((df['res_imag'] ** 2) / len(df)) ** (1 / 2)

    df2 = pd.DataFrame({'linKK_RMSEreal': [kk_real], 'linKK_RMSEimag': [kk_imag]})
    df2['linKK_passFail'] = np.where((df2['linKK_RMSEreal'] <= 0.2) & (df2['linKK_RMSEimag'] <= 0.2), True, False)

    return df2, M, mu, Z_linKK, res_real, res_imag


def get_headcount(file_name):
    file = pd.read_csv(file_name)
    num_points = int(len(file) * .85)
    return num_points


def load_file(file_name, headcount):
    try:
        Z, frequencies = ApplyFits(file_name)
        file = pd.read_csv(file_name)

        if hasattr(Z, 'to_numpy'):
            Z = Z.to_numpy()
        if hasattr(frequencies, 'to_numpy'):
            frequencies = frequencies.to_numpy()

        actual_headcount = min(headcount, len(frequencies), len(Z), len(file))
        frequencies_fit = frequencies[:actual_headcount]
        Z_fit = Z[:actual_headcount]

        try:
            x = file['Re(Z)'][:actual_headcount]
        except KeyError:
            try:
                x = file['Re'][:actual_headcount]
            except KeyError:
                x = np.real(Z[:actual_headcount])

        try:
            y = file['Im(Z)'][:actual_headcount]
        except KeyError:
            try:
                y = file['Im'][:actual_headcount]
            except KeyError:
                y = np.imag(Z[:actual_headcount])

        if hasattr(x, 'to_numpy'):
            x = x.to_numpy()
        if hasattr(y, 'to_numpy'):
            y = y.to_numpy()

        min_length = min(len(frequencies), len(Z), len(x), len(y), actual_headcount)
        frequencies_fit = frequencies[:min_length]
        Z_fit = Z[:min_length]
        x = x[:min_length]
        y = y[:min_length]

        return frequencies, Z, frequencies_fit, Z_fit, x, y

    except Exception as e:
        print(f"Error in load_file: {e}")
        print(f"File name: {file_name}")
        print(f"Headcount: {headcount}")
        try:
            file = pd.read_csv(file_name)
            print(f"Available columns: {file.columns.tolist()}")
            print(f"File shape: {file.shape}")
        except Exception:
            print("Could not read CSV file for debugging")
        raise


def derivative_indicator(x, y):
    dy_dx = np.gradient(-y, x)
    d2y_dx2 = np.gradient(dy_dx, x)
    deriv_ind = np.argsort(abs(d2y_dx2))[-15:]
    last_peak = np.argsort(deriv_ind)[-1]
    R1_peak = deriv_ind[last_peak]
    return R1_peak


def calculate_guess(x, y, frequencies_fit, R1_peak, headcount):
    R0 = x[0] if x[0] > 0 else 1e0
    R1 = x[R1_peak] if x[R1_peak] > 0 else 1e3
    w_freq = frequencies_fit[0]
    C1 = 1 / (2 * math.pi * w_freq * (R1 - R0))

    last_index = min(headcount - 1, len(y) - 1, len(frequencies_fit) - 1)
    Wo1_0 = y[last_index]
    Wo1_1 = 45 / math.sqrt(frequencies_fit[last_index])
    return R0, R1, C1, Wo1_0, Wo1_1


def get_guesses(file_name, headcount):
    try:
        frequencies, Z, frequencies_fit, Z_fit, x, y = load_file(file_name, headcount)
        R1_peak = derivative_indicator(x, y)
        R0, R1, C1, Wo1_0, Wo1_1 = calculate_guess(x, y, frequencies_fit, R1_peak, headcount)
        return R0, R1, C1, Wo1_0, Wo1_1
    except Exception as e:
        print(f"Error in get_guesses: {e}")
        print(f"File: {file_name}")
        raise


def calculate_adaptive_rmse_target(frequencies_fit, Z_fit):
    freq_range = np.log10(np.max(frequencies_fit)) - np.log10(np.min(frequencies_fit))
    Z_magnitude_range = np.log10(np.max(np.abs(Z_fit))) - np.log10(np.min(np.abs(Z_fit)))
    data_points = len(Z_fit)

    Z_magnitude = np.abs(Z_fit)

    if len(Z_fit) > 10:
        high_freq_indices = np.argsort(frequencies_fit)[-10:]
        high_freq_Z_var = np.std(Z_magnitude[high_freq_indices]) / np.mean(Z_magnitude[high_freq_indices])
    else:
        high_freq_Z_var = 0.1

    base_target = 0.08
    freq_adjustment = np.clip(freq_range / 6.0, 0.5, 2.0)
    Z_range_adjustment = np.clip(Z_magnitude_range / 3.0, 0.5, 2.0)
    noise_adjustment = np.clip(high_freq_Z_var * 10, 0.5, 3.0)
    points_adjustment = np.clip(50.0 / data_points, 0.3, 2.0)

    adaptive_target = base_target * freq_adjustment * Z_range_adjustment * noise_adjustment * points_adjustment
    adaptive_target = np.clip(adaptive_target, 0.02, 0.30)
    return adaptive_target


def calculate_frequency_weighted_rmse(params, frequencies_fit, Z_fit):
    R0, R1, Wo1_0, Wo1_1, C1 = params
    temp_randles = mods.Randles(initial_guess=params.tolist())

    with suppress_stdout(suppress=True):
        try:
            temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
            Z_pred = temp_randles.predict(frequencies_fit)
        except Exception:
            return 1e6

    log_residuals = np.log10(Z_fit) - np.log10(Z_pred)
    residuals_real = np.real(log_residuals) / np.abs(np.log10(Z_fit))
    residuals_imag = np.imag(log_residuals) / np.abs(np.log10(Z_fit))

    frequencies_array = np.array(frequencies_fit)
    freq_weights = np.ones_like(frequencies_array)

    phase_data = np.angle(Z_fit, deg=True)
    max_phase_idx = np.argmin(phase_data)
    f_characteristic = frequencies_array[max_phase_idx]

    low_freq_mask = frequencies_array < 0.1
    mid_freq_mask = (frequencies_array >= 0.1) & (frequencies_array < f_characteristic * 100)
    high_freq_mask = (frequencies_array >= f_characteristic * 100) & (frequencies_array < 1e6)
    very_high_freq_mask = frequencies_array >= 1e6

    freq_weights[low_freq_mask] *= 0.5
    freq_weights[mid_freq_mask] *= 2.0
    freq_weights[high_freq_mask] *= 1.0
    freq_weights[very_high_freq_mask] *= 0.1

    phase_actual = np.angle(Z_fit, deg=True)
    phase_pred = np.angle(Z_pred, deg=True)
    phase_diff = phase_actual - phase_pred
    phase_diff = np.where(phase_diff > 180, phase_diff - 360, phase_diff)
    phase_diff = np.where(phase_diff < -180, phase_diff + 360, phase_diff)
    weighted_phase_error = np.abs(phase_diff) * freq_weights / 90.0
    phase_rmse = np.sqrt(np.mean(weighted_phase_error ** 2))

    weighted_rmse_real = np.sqrt(np.mean((residuals_real * freq_weights) ** 2))
    weighted_rmse_imag = np.sqrt(np.mean((residuals_imag * freq_weights) ** 2))

    return 0.3 * (weighted_rmse_real + weighted_rmse_imag) + 0.7 * phase_rmse


def evaluate_fit_quality(Z_fit, Z_pred, frequencies_fit=None, fitted_params=None):
    results = {}

    def r2_score(actual, predicted):
        ss_res = np.sum((actual - predicted) ** 2)
        ss_tot = np.sum((actual - np.mean(actual)) ** 2)
        return 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

    r2_real = r2_score(np.real(Z_fit), np.real(Z_pred))
    r2_imag = r2_score(np.imag(Z_fit), np.imag(Z_pred))
    r2_magnitude = r2_score(np.abs(Z_fit), np.abs(Z_pred))

    if frequencies_fit is not None:
        frequencies_array = np.array(frequencies_fit)
        phase_actual = np.angle(Z_fit, deg=True)
        phase_pred = np.angle(Z_pred, deg=True)

        max_phase_idx = np.argmin(phase_actual)
        f_characteristic = frequencies_array[max_phase_idx]

        phase_weights = np.ones_like(frequencies_array)
        for i, f in enumerate(frequencies_array):
            if f < f_characteristic * 0.01:
                phase_weights[i] = 0.5
            elif f < f_characteristic * 100:
                phase_weights[i] = 2.0
            elif f < 1e6:
                phase_weights[i] = 1.0
            else:
                phase_weights[i] = 0.1

        weighted_phase_res = (phase_actual - phase_pred) * np.sqrt(phase_weights)
        weighted_phase_mean = np.average(phase_actual, weights=phase_weights)
        weighted_phase_tot = (phase_actual - weighted_phase_mean) * np.sqrt(phase_weights)

        ss_res_phase = np.sum(weighted_phase_res ** 2)
        ss_tot_phase = np.sum(weighted_phase_tot ** 2)
        r2_phase = 1 - (ss_res_phase / ss_tot_phase) if ss_tot_phase > 0 else 0
    else:
        r2_phase = r2_score(np.angle(Z_fit), np.angle(Z_pred))

    results.update({
        'r2_real': r2_real,
        'r2_imag': r2_imag,
        'r2_magnitude': r2_magnitude,
        'r2_phase': r2_phase
    })

    real_error = np.mean(np.abs(np.real(Z_fit) - np.real(Z_pred)))
    imag_error = np.mean(np.abs(np.imag(Z_fit) - np.imag(Z_pred)))
    max_real_error = np.max(np.abs(np.real(Z_fit) - np.real(Z_pred)))
    max_imag_error = np.max(np.abs(np.imag(Z_fit) - np.imag(Z_pred)))

    results.update({
        'mean_real_error_ohm': real_error,
        'mean_imag_error_ohm': imag_error,
        'max_real_error_ohm': max_real_error,
        'max_imag_error_ohm': max_imag_error
    })

    try:
        mag_corr = np.corrcoef(np.abs(Z_fit), np.abs(Z_pred))[0, 1]
        phase_corr = np.corrcoef(np.angle(Z_fit), np.angle(Z_pred))[0, 1]
        real_corr = np.corrcoef(np.real(Z_fit), np.real(Z_pred))[0, 1]
        imag_corr = np.corrcoef(np.imag(Z_fit), np.imag(Z_pred))[0, 1]
        results.update({
            'magnitude_correlation': mag_corr,
            'phase_correlation': phase_corr,
            'real_correlation': real_corr,
            'imag_correlation': imag_corr
        })
    except Exception:
        results.update({
            'magnitude_correlation': 0,
            'phase_correlation': 0,
            'real_correlation': 0,
            'imag_correlation': 0
        })

    if fitted_params is not None:
        try:
            R0, R1, Wo1_0, Wo1_1, C1 = fitted_params
            param_score = 100
            param_issues = []

            if R0 <= 0 or R1 <= 0:
                param_issues.append("Negative resistance")
                param_score -= 50
            if R0 > R1:
                param_issues.append("R0 > R1")
                param_score -= 20
            if R0 > 1e6 or R1 > 1e9:
                param_issues.append("Unreasonably high resistance")
                param_score -= 30

            if C1 <= 0:
                param_issues.append("Negative capacitance")
                param_score -= 50
            elif C1 < 1e-12 or C1 > 1e-1:
                param_issues.append("Unreasonable capacitance")
                param_score -= 20

            if Wo1_1 > 1000:
                param_issues.append("Warburg time constant too high")
                param_score -= 20

            results.update({
                'parameter_score': max(0, param_score),
                'parameter_issues': param_issues
            })
        except Exception:
            results.update({
                'parameter_score': 0,
                'parameter_issues': ['Parameter evaluation failed']
            })

    def calculate_composite_score():
        score = 0
        weights = 0
        if 'r2_magnitude' in results and not np.isnan(results['r2_magnitude']):
            score += 20 * max(0, results['r2_magnitude'])
            weights += 20
        if 'r2_phase' in results and not np.isnan(results['r2_phase']):
            score += 60 * max(0, results['r2_phase'])
            weights += 60
        if 'phase_correlation' in results and not np.isnan(results['phase_correlation']):
            score += 10 * max(0, results['phase_correlation'])
            weights += 10
        if 'parameter_score' in results:
            score += 10 * (results['parameter_score'] / 100)
            weights += 10
        return score / weights if weights > 0 else 0

    composite = calculate_composite_score()
    results['composite_score'] = composite
    return results


def robust_randles_fit(randles_model, frequencies_fit, Z_fit, n_attempts=500, metric_type='phase', min_r2_threshold=0.989):
    """
    Monte Carlo sweep with selectable fit quality metrics and R² threshold.
    Enhanced for phase fitting. Adds optional environment driven guardrails:
      - EIS_ATTEMPTS
      - EIS_R2
      - EIS_ROUND_BUDGET_SEC
    """
    # Env overrides
    try:
        n_attempts = int(os.getenv("EIS_ATTEMPTS", str(n_attempts)))
    except Exception:
        pass
    try:
        min_r2_threshold = float(os.getenv("EIS_R2", str(min_r2_threshold)))
    except Exception:
        pass
    try:
        round_budget_sec = float(os.getenv("EIS_ROUND_BUDGET_SEC", "0"))  # 0 disables
    except Exception:
        round_budget_sec = 0.0

    best_fit = None
    best_rmse = np.inf
    best_Z_pred = None
    rmse_history = []
    min_attempts = 5
    convergence_threshold = 0.001

    r2_history = []
    best_r2 = -np.inf

    def calculate_phase_target():
        Z_phase = np.angle(Z_fit, deg=True)
        max_phase = abs(np.min(Z_phase))
        if max_phase < 20:
            return 0.15
        elif max_phase < 40:
            return 0.12
        elif max_phase < 60:
            return 0.08
        elif max_phase < 80:
            return 0.05
        else:
            return 0.03

    try:
        initial_params = randles_model.initial_guess
    except Exception:
        initial_params = [10, 100, 0.01, 0.01, 1e-6]

    log_initial = [np.log10(max(p, 1e-12)) for p in initial_params]

    def sample_around_initial(iteration_round=1):
        sweep_ranges = [4, 4, 3, 2, 4] if iteration_round == 1 else [2, 2, 2, 1, 2]
        sampled_log_params = []
        for log_center, sweep_range in zip(log_initial, sweep_ranges):
            log_param = np.random.uniform(log_center - sweep_range, log_center + sweep_range)
            sampled_log_params.append(log_param)
        linear_params = [10 ** log_p for log_p in sampled_log_params]
        return linear_params, sampled_log_params

    def evaluate_attempt(Z_fit, temp_Z_pred, sampled_params):
        fit_quality = evaluate_fit_quality(Z_fit, temp_Z_pred, frequencies_fit, sampled_params)
        current_r2 = fit_quality['r2_magnitude']

        if metric_type == 'phase':
            rmse_real, rmse_imag, _, _ = calcValues(Z_fit, temp_Z_pred)
            frequencies_array = np.array(frequencies_fit)
            phase_actual = np.angle(Z_fit, deg=True)
            phase_pred = np.angle(temp_Z_pred, deg=True)
            phase_diff = phase_actual - phase_pred
            phase_diff = np.where(phase_diff > 180, phase_diff - 360, phase_diff)
            phase_diff = np.where(phase_diff < -180, phase_diff + 360, phase_diff)
            freq_weights = np.ones_like(frequencies_array)
            high_freq_mask = frequencies_array > 1e5
            freq_weights[high_freq_mask] *= 0.1
            weighted_phase_rmse = np.sqrt(np.mean((phase_diff * freq_weights) ** 2))
            return rmse_real + rmse_imag + 0.1 * weighted_phase_rmse, 'lower_better', current_r2

        elif metric_type == 'r2':
            r2_phase = fit_quality.get('r2_phase', 0)
            combined_r2 = 0.5 * current_r2 + 0.5 * r2_phase
            return combined_r2, 'higher_better', current_r2

        elif metric_type == 'composite':
            return fit_quality['composite_score'], 'higher_better', current_r2

        elif metric_type == 'absolute':
            max_error = max(fit_quality['max_real_error_ohm'], fit_quality['max_imag_error_ohm'])
            return max_error, 'lower_better', current_r2

        rmse_real, rmse_imag, _, _ = calcValues(Z_fit, temp_Z_pred)
        return rmse_real + rmse_imag, 'lower_better', current_r2

    if metric_type == 'phase':
        target_value = calculate_phase_target()
        comparison_better = lambda current, target: current <= target
    elif metric_type == 'r2':
        target_value = min_r2_threshold
        comparison_better = lambda current, target: current >= target
    elif metric_type == 'composite':
        target_value = 85
        comparison_better = lambda current, target: current >= target
    elif metric_type == 'absolute':
        target_value = 1000
        comparison_better = lambda current, target: current <= target

    def run_fitting_attempts(round_num=1, max_attempts=None):
        nonlocal best_fit, best_rmse, best_Z_pred, rmse_history, best_r2, r2_history
        attempts_to_use = max_attempts if max_attempts else n_attempts

        emit_progress("prepare", total_attempts=attempts_to_use, round=round_num)
        round_start = time.perf_counter()

        best_metric_value = np.inf if metric_type in ['phase', 'absolute'] else -np.inf

        for attempt in range(attempts_to_use):
            emit_progress("attempt", attempt=attempt + 1, total_attempts=attempts_to_use, round=round_num)

            # Optional wall clock guardrail
            if round_budget_sec > 0 and attempt >= min_attempts:
                if time.perf_counter() - round_start >= round_budget_sec:
                    break

            try:
                if attempt == 0 and round_num == 1:
                    sampled_params = initial_params
                else:
                    sampled_params, _ = sample_around_initial(round_num)

                R0, R1, Wo1_0, Wo1_1, C1 = sampled_params
                if R0 > R1 or Wo1_1 > 100:
                    continue

                temp_randles = mods.Randles(initial_guess=sampled_params)
                with suppress_stdout(suppress=True):
                    temp_fit = temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                    temp_Z_pred = temp_randles.predict(frequencies_fit)

                metric_value, direction, current_r2 = evaluate_attempt(Z_fit, temp_Z_pred, sampled_params)
                r2_history.append(current_r2)

                rmse_real, rmse_imag, _, _ = calcValues(Z_fit, temp_Z_pred)
                total_rmse = rmse_real + rmse_imag
                rmse_history.append(total_rmse)

                r2_meets_threshold = current_r2 >= min_r2_threshold
                is_better = False
                if direction == 'lower_better':
                    if metric_value < best_metric_value and r2_meets_threshold:
                        is_better = True
                        best_metric_value = metric_value
                else:
                    if metric_value > best_metric_value and r2_meets_threshold:
                        is_better = True
                        best_metric_value = metric_value

                if current_r2 > best_r2:
                    best_r2 = current_r2

                if is_better:
                    best_rmse = total_rmse
                    best_fit = temp_fit
                    best_Z_pred = temp_Z_pred

                if comparison_better(best_metric_value, target_value) and r2_meets_threshold:
                    return True

            except Exception:
                rmse_history.append(np.inf)
                r2_history.append(0)
                continue

            if attempt >= min_attempts - 1 and len(rmse_history) > min_attempts:
                recent_best = min(rmse_history[-min_attempts:])
                previous_best = min(rmse_history[:-min_attempts])
                if previous_best > 0:
                    improvement = (previous_best - recent_best) / previous_best
                    if improvement < 0.001:
                        break

        return False

    success = run_fitting_attempts(round_num=1)
    if not success and best_r2 < min_r2_threshold:
        success = run_fitting_attempts(round_num=2, max_attempts=50)
        if not success and best_r2 < min_r2_threshold:
            success = run_fitting_attempts(round_num=3, max_attempts=100)

    emit_progress("done")

    if best_fit is None or best_Z_pred is None:
        return None, None, np.inf

    return best_fit, best_Z_pred, best_rmse


def get_parameter_bounds():
    log_bounds = [
        (-3, 6),     # R0: 1e-3 to 1e6 Ω
        (2, 8),      # R1: 1e2 to 1e8 Ω
        (-8, 2),     # Wo1_0: 1e-8 to 1e2
        (-8, 2),     # Wo1_1: 1e-8 to 1e2
        (-12, -1)    # C1: 1e-12 to 1e-1 F
    ]
    return log_bounds


def optimize_randles_parameters(frequencies_fit, Z_fit, R0_guess, R1_guess,
                                Wo1_0_guess, Wo1_1_guess, C1_guess, objective_type='rmse'):
    def calculate_r2_objective(log_params):
        try:
            params = 10 ** log_params
            temp_randles = mods.Randles(initial_guess=params.tolist())
            with suppress_stdout(suppress=True):
                temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                Z_pred = temp_randles.predict(frequencies_fit)
            ss_res = np.sum((np.abs(Z_fit) - np.abs(Z_pred)) ** 2)
            ss_tot = np.sum((np.abs(Z_fit) - np.mean(np.abs(Z_fit))) ** 2)
            r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
            return -r2
        except Exception:
            return 1e6

    def calculate_hybrid_objective(log_params):
        try:
            params = 10 ** log_params
            R0, R1, Wo1_0, Wo1_1, C1 = params
            temp_randles = mods.Randles(initial_guess=params.tolist())
            with suppress_stdout(suppress=True):
                temp_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                Z_pred = temp_randles.predict(frequencies_fit)
            ss_res = np.sum((np.abs(Z_fit) - np.abs(Z_pred)) ** 2)
            ss_tot = np.sum((np.abs(Z_fit) - np.mean(np.abs(Z_fit))) ** 2)
            r2 = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

            param_penalty = 0
            if R0 <= 0 or R1 <= 0:
                param_penalty += 0.1
            if C1 <= 0:
                param_penalty += 0.1
            if R0 > R1:
                param_penalty += 0.05

            objective = -r2 + param_penalty
            return objective
        except Exception:
            return 1e6

    if objective_type == 'r2':
        objective_function = calculate_r2_objective
    elif objective_type == 'hybrid':
        objective_function = calculate_hybrid_objective
    else:
        objective_function = lambda log_params: calculate_frequency_weighted_rmse(10 ** log_params, frequencies_fit, Z_fit)

    initial_log_params = np.log10([R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess])
    log_bounds = get_parameter_bounds()

    methods = ['L-BFGS-B'] if objective_type == 'rmse' else ['L-BFGS-B', 'TNC', 'SLSQP']
    best_result = None
    best_objective = np.inf

    for method in methods:
        try:
            result = minimize(
                objective_function,
                initial_log_params,
                bounds=log_bounds,
                method=method,
                options={'maxiter': 1000, 'ftol': 1e-12}
            )
            if result.success and result.fun < best_objective:
                best_result = result
                best_objective = result.fun
        except Exception:
            continue

    if best_result is not None and best_result.success:
        optimal_params = 10 ** best_result.x
        return optimal_params, True
    else:
        return np.array([R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess]), False


def ApplyRandlesFits_Calculate(file_name, R0_guess, R1_guess,
                               Wo1_0_guess, Wo1_1_guess,
                               C1_guess, head_count, global_opt,
                               fit_metric='phase', optimization_target='phase_weighted',
                               min_r2_threshold=0.995):
    basename = os.path.basename(file_name)
    with suppress_stdout(suppress=True):
        results_df, M, mu, Z_linKK, res_real, res_imag = EIS_validate(file_name)
        linkk_passfail = results_df['linKK_passFail'][0]

    frequencies, Z, frequencies_fit, Z_fit, x, y = load_file(file_name, head_count)

    optimal_params, optimization_success = optimize_randles_parameters(
        frequencies_fit, Z_fit, R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess,
        objective_type=optimization_target
    )

    randles = mods.Randles(initial_guess=optimal_params.tolist())
    randles_fit, Z_randles, robust_rmse = robust_randles_fit(
        randles, frequencies_fit, Z_fit,
        n_attempts=200,
        metric_type=fit_metric,
        min_r2_threshold=min_r2_threshold
    )

    if randles_fit is None or Z_randles is None:
        fallback_thresholds = [0.98, 0.95, 0.90, 0.80, 0.70, 0.50, 0.0]
        for threshold in fallback_thresholds:
            if threshold < min_r2_threshold:
                n_attempts = 50 if threshold > 0.8 else 30
                randles_fit, Z_randles, robust_rmse = robust_randles_fit(
                    randles, frequencies_fit, Z_fit,
                    n_attempts=n_attempts,
                    metric_type=fit_metric,
                    min_r2_threshold=threshold
                )
                if randles_fit is not None and Z_randles is not None:
                    min_r2_threshold = threshold
                    break

        if randles_fit is None or Z_randles is None:
            try:
                direct_randles = mods.Randles(initial_guess=optimal_params.tolist())
                randles_fit = direct_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                Z_randles = direct_randles.predict(frequencies_fit)
                if randles_fit is not None:
                    fit_quality = evaluate_fit_quality(Z_fit, Z_randles, frequencies_fit, optimal_params)
                    actual_r2 = fit_quality.get('r2_magnitude', 0)
                    min_r2_threshold = actual_r2
                    robust_rmse = np.inf
            except Exception as e:
                print(f"Direct fit failed: {e}")

        if randles_fit is None or Z_randles is None:
            generic_params = [10, 10000, 0.01, 1.0, 1e-6]
            try:
                generic_randles = mods.Randles(initial_guess=generic_params)
                randles_fit = generic_randles.fit(frequencies_fit, Z_fit, weight_by_modulus=True)
                Z_randles = generic_randles.predict(frequencies_fit)
                if randles_fit is not None:
                    optimal_params = np.array(generic_params)
                    min_r2_threshold = 0.0
                    robust_rmse = np.inf
            except Exception:
                pass

        if randles_fit is None or Z_randles is None:
            return {
                'basename': basename,
                'randles_fit': None,
                'Z_randles': None,
                'frequencies_fit': frequencies_fit,
                'Z_fit': Z_fit,
                'frequencies': frequencies,
                'res_real': res_real,
                'res_imag': res_imag,
                'rmse_real': np.inf,
                'rmse_imag': np.inf,
                'mu': mu,
                'M': M,
                'linkk_passfail': linkk_passfail,
                'output_df': None,
                'file_name': file_name,
                'head_count': head_count,
                'optimization_success': False,
                'robust_rmse': np.inf,
                'final_quality': {'r2_magnitude': 0, 'r2_phase': 0},
                'r2_threshold_met': False,
                'min_r2_threshold': min_r2_threshold,
                'fitting_failed': True
            }

    rmse_real, rmse_imag, Z_real, Z_imag = calcValues(Z_fit, Z_randles)
    final_quality = evaluate_fit_quality(Z_fit, Z_randles, frequencies_fit, optimal_params)

    if fit_metric == 'phase' or optimization_target == 'phase_weighted':
        r2_to_check = final_quality.get('r2_combined', final_quality['r2_magnitude'])
    else:
        r2_to_check = final_quality['r2_magnitude']
    r2_threshold_met = r2_to_check >= min_r2_threshold

    stats_df = [[file_name, head_count, res_real, res_imag, mu, M, linkk_passfail, rmse_real, rmse_imag]]
    stats_df_names = [
        'EIS_file', 'head_count', 'linKK_RMSEreal', 'linKK_RMSEimag',
        'mu', 'Circuits', 'Lin-KK_PassFail', 'RMSE_real', 'RMSE_imag'
    ]
    stats_df = pd.DataFrame(stats_df, columns=stats_df_names)

    cols = ['InitialGuess.R0', 'InitialGuess.R1', 'InitialGuess.Wo1_0',
            'InitialGuess.Wo1_1', 'InitialGuess.C1', 'FittedValue.R0',
            'FittedValue.R1', 'FittedValue.Wo1_0', 'FittedValue.Wo1_1',
            'FittedValue.C1', 'FittedValue.R0_stDev', 'FittedValue.R1_stDev',
            'FittedValue.Wo1_0_stDev', 'FittedValue.Wo1_1_stDev', 'FittedValue.C1_stDev']

    output_df = EISresult_cleaner(randles_fit)
    output_df[cols] = output_df[cols].apply(pd.to_numeric, errors='coerce', axis=1)
    output_df = pd.concat([stats_df, output_df], axis=1)

    output_df['optimization_success'] = optimization_success
    output_df['optimization_target'] = optimization_target
    output_df['fit_metric'] = fit_metric
    output_df['robust_rmse'] = robust_rmse
    output_df['r2_magnitude'] = final_quality['r2_magnitude']
    output_df['r2_phase'] = final_quality.get('r2_phase', 0)
    output_df['r2_combined'] = final_quality.get('r2_combined', final_quality['r2_magnitude'])
    output_df['composite_score'] = final_quality['composite_score']

    output_df['actual_r2_threshold'] = min_r2_threshold
    output_df['used_fallback'] = (min_r2_threshold < 0.995) or (robust_rmse == np.inf)
    output_df['fallback_type'] = 'none'
    if min_r2_threshold < 0.995:
        output_df['fallback_type'] = 'reduced_threshold'
    if robust_rmse == np.inf:
        output_df['fallback_type'] = 'direct_fit'

    return {
        'basename': basename, 'randles_fit': randles_fit, 'Z_randles': Z_randles,
        'frequencies_fit': frequencies_fit, 'Z_fit': Z_fit, 'frequencies': frequencies,
        'res_real': res_real, 'res_imag': res_imag, 'rmse_real': rmse_real,
        'rmse_imag': rmse_imag, 'mu': mu, 'M': M, 'linkk_passfail': linkk_passfail,
        'output_df': output_df, 'file_name': file_name, 'head_count': head_count,
        'optimization_success': optimization_success, 'robust_rmse': robust_rmse,
        'final_quality': final_quality, 'r2_threshold_met': r2_threshold_met,
        'min_r2_threshold': min_r2_threshold, 'fitting_failed': False
    }


def ApplyRandlesFits_Plot(file_name, calc_results, output_path, save_file):
    basename = os.path.basename(file_name)
    randles_fit = calc_results['randles_fit']
    frequencies_fit = calc_results['frequencies_fit']
    Z_fit = calc_results['Z_fit']
    frequencies = calc_results['frequencies']
    res_real = calc_results['res_real']
    res_imag = calc_results['res_imag']
    rmse_real = calc_results['rmse_real']
    rmse_imag = calc_results['rmse_imag']
    mu = calc_results['mu']
    M = calc_results['M']
    output_df = calc_results['output_df']
    robust_rmse = calc_results['robust_rmse']

    final_quality = calc_results.get('final_quality', {})
    r2_mag = final_quality.get('r2_magnitude', 0)
    r2_phase = final_quality.get('r2_phase', 0)
    r2_combined = final_quality.get('r2_combined', r2_mag)
    r2_threshold_met = calc_results.get('r2_threshold_met', False)
    min_r2_threshold = calc_results.get('min_r2_threshold', 0.989)

    threshold_status = "✓ PASS" if r2_threshold_met else "✗ FAIL"
    threshold_text = f"R² Threshold ({min_r2_threshold:.1%}): {threshold_status}"

    if save_file:
        fig, ax2 = plt.subplots(figsize=(5, 5))
        randles_fit.plot(f_data=frequencies_fit, Z_data=Z_fit, kind='bode', units='\Omega')
        ax2.legend(['Data', 'Fitted'], loc='upper right')
        ax2.set_title('Bode Plot', size=11)
        plt.axis('auto')
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size=8)
        plt.tight_layout()
        file_namePlot = basename.replace(' ', '_').replace('.csv', '_BodePlot_randles.png')
        plt.savefig(output_path + r'\\' + file_namePlot, dpi=600, format='png')
        plt.close(fig)

        fig, ax3 = plt.subplots(figsize=(5, 5))
        ax3.residual_plot = viz.plot_residuals(ax3, frequencies, res_real, res_imag, y_limits=(-5, 5))
        ax3.set_title(fr'Lin-KK Residuals, $\mu$ = {mu}, # circuits = {M}', size=10)
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size=8)
        plt.tight_layout()
        file_namePlot = basename.replace(' ', '_').replace('.csv', '_LinKKResiduals_randles.png')
        plt.savefig(output_path + r'\\' + file_namePlot, dpi=600, format='png')
        plt.close(fig)

        fig, ax1 = plt.subplots(figsize=(5, 5))
        ax1.nyquist_plot = randles_fit.plot(ax1, f_data=frequencies_fit, Z_data=Z_fit, kind='nyquist', units='\Omega')
        ax1.legend(['Data', 'Fitted'], loc='upper left')
        ax1.set_aspect('auto')
        ax1.set_title('Nyquist Plot', size=10)
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size=8)
        plt.tight_layout()
        file_namePlot = basename.replace(' ', '_').replace('.csv', '_randles.png')
        plt.savefig(output_path + r'\\' + file_namePlot, dpi=600, format='png')
        plt.close(fig)

        file_nameCSV = basename.replace(' ', '_').replace('.csv', '__randles.csv')
        output_df.to_csv(output_path + r'\\' + file_nameCSV, index=False)

        if not r2_threshold_met:
            print(f'WARNING: R² threshold not met! ({r2_combined:.3f} < {min_r2_threshold:.3f})')
    else:
        fig, ax = plt.subplots(figsize=(5, 5), squeeze=True)
        ax.nyquist_plot = randles_fit.plot(ax, f_data=frequencies_fit, Z_data=Z_fit, kind='nyquist', units='\Omega')
        ax.legend(['Data', 'Fitted'], loc='upper right')
        ax.set_title(f'Nyquist Plot (NOT SAVED)\nRMSE: Real={rmse_real:.4f}, Imag={rmse_imag:.4f}\nR²: Mag={r2_mag:.3f}, Phase={r2_phase:.3f}, Combined={r2_combined:.3f}, {threshold_text}', size=10)
        ax.set_aspect('auto')
        plt.suptitle('\n'.join(textwrap.wrap(f'Sample: {basename}', 80)), size=10)
        plt.tight_layout()
        if not r2_threshold_met:
            print(f'WARNING: R² threshold not met! ({r2_combined:.3f} < {min_r2_threshold:.3f})')


def ApplyRandlesFits(file_name, output_path, R0_guess, R1_guess,
                     Wo1_0_guess, Wo1_1_guess, C1_guess, head_count, save_file, global_opt):
    calc_results = ApplyRandlesFits_Calculate(
        file_name, R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess, head_count, global_opt,
        fit_metric='phase', optimization_target='phase_weighted', min_r2_threshold=0.989
    )
    ApplyRandlesFits_Plot(file_name, calc_results, output_path, save_file)
    return calc_results['output_df']


def apply_randles_fits_wrapper(file_name, output, R0_guess, R1_guess, Wo1_0_guess, Wo1_1_guess, C1_guess, head_count, save_file, global_opt):
    head_count = int(head_count)
    ApplyRandlesFits(file_name, output_path=output,
                     R0_guess=R0_guess, R1_guess=R1_guess,
                     Wo1_0_guess=Wo1_0_guess, Wo1_1_guess=Wo1_1_guess,
                     C1_guess=C1_guess, head_count=head_count,
                     save_file=save_file, global_opt=global_opt)


def run_EIS_analysis(filename, save_path):
    headcount = get_headcount(filename)
    R0, R1, C1, Wo1_0, Wo1_1 = get_guesses(filename, headcount)
    apply_randles_fits_wrapper(filename, save_path, R0, R1, Wo1_0, Wo1_1, C1, headcount, True, False)


# -------------------------
# Array-based cycle fitting helpers
# -------------------------

def extract_arrays_from_cycle(cycle_obj):
    vars_ = cycle_obj.get("data_array_variables") or cycle_obj.get("variables")
    vals_ = cycle_obj.get("data_array_values") or cycle_obj.get("values")

    def build_df(arrs):
        freq = arrs.get("freq") or arrs.get("Frequency") or arrs.get("frequency")
        re_ = arrs.get("Re") or arrs.get("Re(Z)") or arrs.get("Zreal")
        im_ = arrs.get("Im") or arrs.get("Im(Z)") or arrs.get("Zimag")
        if freq is None or re_ is None or im_ is None:
            raise ValueError("Missing freq/Re/Im arrays in cycle object")
        return pd.DataFrame({
            "freq": pd.to_numeric(pd.Series(freq), errors="coerce"),
            "Re": pd.to_numeric(pd.Series(re_), errors="coerce"),
            "Im": pd.to_numeric(pd.Series(im_), errors="coerce"),
        })

    if isinstance(vars_, list) and isinstance(vals_, list) and len(vars_) == len(vals_):
        arrs = {v: vals_[i] for i, v in enumerate(vars_)}
        return build_df(arrs)

    av = cycle_obj.get("all_variables")
    avv = cycle_obj.get("all_values")
    if isinstance(av, list) and isinstance(avv, list) and len(av) == len(avv):
        arrs = {v: avv[i] for i, v in enumerate(av)}
        return build_df(arrs)

    raise ValueError("Missing freq/Re/Im arrays in cycle object")


def sanitize_cycle_df(df):
    d = df.copy()
    d["freq"] = pd.to_numeric(d["freq"], errors="coerce")
    d["Re"] = pd.to_numeric(d["Re"], errors="coerce")
    d["Im"] = pd.to_numeric(d["Im"], errors="coerce")
    d = d.dropna(subset=["freq", "Re", "Im"])
    d = d[d["freq"] > 0]
    d = d.sort_values("freq", ascending=False).reset_index(drop=True)
    d = d.loc[~d["freq"].duplicated(keep="first")].reset_index(drop=True)
    return d


def robust_guess(x, y, freqs_fit):
    pos_x = x[np.isfinite(x)]
    pos_x = pos_x[pos_x > 0]
    if pos_x.size:
        r0 = float(np.percentile(pos_x, 5))
        r1 = float(np.percentile(pos_x, 95))
        if r1 <= r0:
            r1 = r0 * 1.5
    else:
        r0 = 1.0
        r1 = 1.5

    fpos = freqs_fit[(freqs_fit > 0) & np.isfinite(freqs_fit)]
    if fpos.size:
        f_mid = float(10 ** np.mean(np.log10(fpos)))
    else:
        f_mid = float(freqs_fit[0]) if freqs_fit.size else 1.0

    delta_r = max(r1 - r0, 1e-6)
    c1 = 1.0 / (2 * np.pi * f_mid * delta_r)
    c1 = float(np.clip(c1, 1e-12, 1e-1))
    wo1_0 = float(np.nanmax(np.abs(y))) if y.size else 0.01
    wo1_1 = float(45.0 / np.sqrt(fpos.min())) if fpos.size else 1.0
    wo1_1 = float(np.clip(wo1_1, 1e-8, 1e2))
    return r0, r1, c1, wo1_0, wo1_1


def build_richer_arrays_from_sanitized(df_s: pd.DataFrame):
    return (
        ["freq", "Re", "Im"],
        [
            df_s["freq"].tolist(),
            df_s["Re"].tolist(),
            df_s["Im"].tolist(),
        ],
    )


def fit_cycle_from_cycle_obj(cycle_obj):
    df_raw = extract_arrays_from_cycle(cycle_obj)
    df_s = sanitize_cycle_df(df_raw)

    if df_s.shape[0] < 10:
        raise RuntimeError("Too few valid points after sanitation")

    freqs = df_s["freq"].to_numpy()
    re = df_s["Re"].to_numpy()
    im = df_s["Im"].to_numpy()
    z = re - 1j * im

    headcount = max(1, int(len(df_s) * 0.85))
    # Env cap to avoid giant cycles dominating runtime
    try:
        max_pts_env = os.getenv("EIS_MAX_POINTS")
        if max_pts_env:
            max_pts = int(max_pts_env)
            headcount = min(headcount, max_pts)
    except Exception:
        pass
    headcount = min(headcount, len(df_s))

    freqs_fit = freqs[:headcount]
    z_fit = z[:headcount]
    x = re[:headcount]
    y = im[:headcount]

    try:
        r1_peak = derivative_indicator(x, y)
        r0, r1, c1, wo1_0, wo1_1 = calculate_guess(x, y, freqs_fit, r1_peak, headcount)
        invalid = (
            not np.isfinite(r0) or not np.isfinite(r1) or not np.isfinite(c1)
            or r0 <= 0 or r1 <= 0 or c1 <= 0 or r1 <= r0
        )
        if invalid:
            r0, r1, c1, wo1_0, wo1_1 = robust_guess(x, y, freqs_fit)
    except Exception:
        r0, r1, c1, wo1_0, wo1_1 = robust_guess(x, y, freqs_fit)

    optimal_params, _ = optimize_randles_parameters(
        freqs_fit, z_fit, r0, r1, wo1_0, wo1_1, c1, objective_type="rmse"
    )
    randles_model = mods.Randles(initial_guess=optimal_params.tolist())

    randles_fit, z_pred, robust_rmse = robust_randles_fit(
        randles_model=randles_model,
        frequencies_fit=freqs_fit,
        Z_fit=z_fit,
        n_attempts=200,
        metric_type="phase",
        min_r2_threshold=0.989
    )
    if randles_fit is None or z_pred is None:
        trials = [
            ("phase", 0.98, 120),
            ("r2", 0.98, 100),
            ("phase", 0.95, 80),
            ("absolute", 0.0, 60),
        ]
        for metric, thr, attempts in trials:
            randles_fit, z_pred, robust_rmse = robust_randles_fit(
                randles_model=randles_model,
                frequencies_fit=freqs_fit,
                Z_fit=z_fit,
                n_attempts=attempts,
                metric_type=metric,
                min_r2_threshold=thr
            )
            if randles_fit is not None and z_pred is not None:
                break

    if randles_fit is None or z_pred is None:
        raise RuntimeError("Robust Randles fit failed after fallbacks")

    final_quality = evaluate_fit_quality(z_fit, z_pred, freqs_fit, optimal_params)
    r2_mag = float(final_quality.get("r2_magnitude", 0.0))
    r2_phase = float(final_quality.get("r2_phase", 0.0))
    rmse_real, rmse_imag, _, _ = calcValues(z_fit, z_pred)

    def flatten_summary_df(df_summary):
        out = {}
        if df_summary is None or df_summary.empty:
            return out
        for c in df_summary.columns:
            if c.startswith(("InitialGuess.", "FittedValue.")):
                df_summary[c] = pd.to_numeric(df_summary[c], errors="coerce")
        row = df_summary.iloc[0].to_dict()
        for k, v in row.items():
            if k.startswith(("InitialGuess.", "FittedValue.")) and isinstance(v, (int, float, np.floating)):
                out[k] = float(v)
        return out

    try:
        df_summary = EISresult_cleaner(randles_fit)
        fitted_dict = flatten_summary_df(df_summary)
        if not fitted_dict:
            raise RuntimeError("Empty fitted summary")
    except Exception:
        fitted_dict = {
            "FittedValue.R0": float(optimal_params[0]),
            "FittedValue.R1": float(optimal_params[1]),
            "FittedValue.Wo1_0": float(optimal_params[2]),
            "FittedValue.Wo1_1": float(optimal_params[3]),
            "FittedValue.C1": float(optimal_params[4]),
        }

    fit_array_variables = [
        "freq", "Z_pred_real", "Z_pred_imag", "Z_measured_real", "Z_measured_imag"
    ]
    fit_array_values = [
        freqs_fit.tolist(),
        np.real(z_pred).tolist(),
        np.imag(z_pred).tolist(),
        np.real(z_fit).tolist(),
        np.imag(z_fit).tolist(),
    ]

    if cycle_obj.get("all_variables") and cycle_obj.get("all_values") and \
       len(cycle_obj["all_variables"]) == len(cycle_obj["all_values"]) and len(cycle_obj["all_variables"]) > 0:
        data_vars_out = cycle_obj["all_variables"]
        data_vals_out = cycle_obj["all_values"]
    else:
        source_vars = cycle_obj.get("data_array_variables") or cycle_obj.get("variables") or []
        source_vals = cycle_obj.get("data_array_values") or cycle_obj.get("values") or []
        if isinstance(source_vars, list) and isinstance(source_vals, list) and len(source_vars) == len(source_vals) and len(source_vars) > 0:
            data_vars_out = source_vars
            data_vals_out = source_vals
        else:
            data_vars_out, data_vals_out = build_richer_arrays_from_sanitized(df_s)

    return {
        **fitted_dict,
        "RMSE_real": float(rmse_real),
        "RMSE_imag": float(rmse_imag),
        "r2_magnitude": r2_mag,
        "r2_phase": r2_phase,
        "fit_status": "ok",
        "fit_array_variables": fit_array_variables,
        "fit_array_values": fit_array_values,
        "data_array_variables": data_vars_out,
        "data_array_values": data_vals_out,
    }
