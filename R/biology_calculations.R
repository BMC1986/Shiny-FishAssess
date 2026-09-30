# Helper functions for plotting biological parameters -------------------------------------------

# Calculate von Bertalanffy Growth (no change from previous)
calculate_vb_lengths <- function(Linf, K, t0, ages) {
  if (any(is.na(c(Linf, K, t0))) || !is.numeric(Linf) || !is.numeric(K) || !is.numeric(t0)) {
    return(rep(NA, length(ages)))
  }
  Linf * (1 - exp(-K * (as.numeric(ages) - t0)))
}

# Calculate Schnute Growth
# Parameters based on: y1, y2, a (kgrowth), b (gamma/shape), t1, t2
calculate_schnute_lengths <- function(y1, y2, a_rate, b_shape, t1, t2, ages) {
  # Ensure all parameters are numeric and ages is numeric
  params <- c(y1, y2, a_rate, b_shape, t1, t2)
  if (any(sapply(params, function(p) !is.numeric(p) || is.na(p))) || !is.numeric(ages)) {
    return(rep(NA, length(ages)))
  }
  if (t1 == t2) { # Avoid division by zero if t1 equals t2
    return(rep(y1, length(ages))) # Or handle as an error/warning
  }
  
  len_at_age <- numeric(length(ages))
  
  # Term for proportion of growth between t1 and t2
  # (1 - exp(-a_rate * (age - t1))) / (1 - exp(-a_rate * (t2 - t1)))
  
  denominator_val <- 1 - exp(-a_rate * (t2 - t1))
  
  for (i in seq_along(ages)) {
    age <- ages[i]
    frac_val <- NA
    
    if (abs(a_rate) < 1e-6) { # If 'a_rate' is effectively zero, linear interpolation proportion
      if ((t2 - t1) == 0) frac_val <- 0.5 # Or handle error, should be caught by t1==t2 above
      else frac_val <- (age - t1) / (t2 - t1)
    } else if (abs(denominator_val) < 1e-9) { # Denominator is effectively zero (should be rare if a_rate != 0 and t1 != t2)
      # This case implies -a_rate * (t2-t1) is a multiple of 2*pi*i for complex exp, or very large for real exp
      # Or if a_rate * (t2-t1) is very large, so exp(-term) -> 0, making denom = 1.
      # If denom is truly ~0 because exp(-a(t2-t1)) ~ 1 (and a!=0), implies a(t2-t1) ~0, handled by above.
      # Let's assume if denom is ~0 and a_rate !=0, it means the growth period is very short relative to rate,
      # or rate is such that it completes/saturates.
      # Simplified: if denom is zero, it might mean full transition or linear.
      # Using the linear approximation if denominator is problematic.
      if ((t2 - t1) == 0) frac_val <- 0.5
      else frac_val <- (age - t1) / (t2 - t1) # Fallback, though ideally covered
    } else {
      numerator_val <- 1 - exp(-a_rate * (age - t1))
      frac_val <- numerator_val / denominator_val
    }
    
    # Ensure frac_val is within reasonable bounds if it's an interpolator, typically [0,1] for ages between t1 and t2
    # However, Schnute can extrapolate, so we allow it.
    
    if (abs(b_shape) < 1e-6) { # b_shape (gamma) is effectively zero
      if (y1 <= 0 || y2 <= 0) { # Log form requires positive y1, y2
        len_at_age[i] <- NA # Or handle appropriately
        next
      }
      len_at_age[i] <- y1 * exp(log(y2 / y1) * frac_val)
    } else { # b_shape is not zero
      # Handle y1 = 0 case specifically, as y1^b_shape can be problematic (e.g. 0^negative)
      if (abs(y1) < 1e-9 && b_shape > 0) { # If y1 is zero and b_shape is positive
        len_at_age[i] <- y2 * (frac_val ^ (1 / b_shape))
      } else if (abs(y1) < 1e-9 && b_shape <= 0) { # y1 is zero and b_shape is not positive
        len_at_age[i] <- NA # Undefined or problematic
      }
      else { # y1 is not zero
        term1_pow_b <- y1^b_shape
        term2_pow_b <- y2^b_shape
        inside_bracket <- term1_pow_b + (term2_pow_b - term1_pow_b) * frac_val
        
        if(inside_bracket < 0 && (1/b_shape) %% 1 != 0 && b_shape %% 2 == 0) { # e.g. (-ve)^(0.5)
          len_at_age[i] <- NA
        } else if (inside_bracket == 0 && (1/b_shape) < 0) { # e.g. 0^(-ve)
          len_at_age[i] <- Inf # or NA
        }
        else {
          # sign(inside_bracket) needed if raising negative number to non-integer power
          len_at_age[i] <- sign(inside_bracket) * (abs(inside_bracket)^(1 / b_shape))
        }
      }
    }
  }
  return(len_at_age)
}


# Weight at Length and Maturity functions (no change from previous response)
calculate_weight_at_length <- function(a, b, len_values) {
  if (any(is.na(c(a, b))) || !is.numeric(a) || !is.numeric(b)) {
    return(rep(NA, length(len_values)))
  }
  a * (as.numeric(len_values)^b)
}

calculate_logistic_maturity <- function(L50_or_A50, L95_or_A95, values) {
  if (any(is.na(c(L50_or_A50, L95_or_A95))) ||
      !is.numeric(L50_or_A50) || !is.numeric(L95_or_A95) ||
      (L95_or_A95 - L50_or_A50 == 0)) {
    return(rep(NA, length(values)))
  }
  1 / (1 + exp(-log(19) * (as.numeric(values) - L50_or_A50) / (L95_or_A95 - L50_or_A50)))
}