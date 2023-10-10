package it.gov.pagopa.debtposition.util;

import org.springframework.lang.Nullable;

public enum CustomHttpStatus {
	
		// 2xx Custom Success
		IN_PROGRESS(209, Series.SUCCESSFUL, "In Progress State");

		private static final CustomHttpStatus[] VALUES;

		static {
			VALUES = values();
		}


		private final int value;

		private final Series series;

		private final String reasonPhrase;

		CustomHttpStatus(int value, Series series, String reasonPhrase) {
			this.value = value;
			this.series = series;
			this.reasonPhrase = reasonPhrase;
		}


		/**
		 * Return the integer value of this status code.
		 */
		public int value() {
			return this.value;
		}

		/**
		 * Return the HTTP status series of this status code.
		 */
		public Series series() {
			return this.series;
		}

		/**
		 * Return the reason phrase of this status code.
		 */
		public String getReasonPhrase() {
			return this.reasonPhrase;
		}

		
		public boolean is1xxInformational() {
			return (series() == Series.INFORMATIONAL);
		}

		
		public boolean is2xxSuccessful() {
			return (series() == Series.SUCCESSFUL);
		}

		
		public boolean is3xxRedirection() {
			return (series() == Series.REDIRECTION);
		}

		
		public boolean is4xxClientError() {
			return (series() == Series.CLIENT_ERROR);
		}

		
		public boolean is5xxServerError() {
			return (series() == Series.SERVER_ERROR);
		}

		
		public boolean isError() {
			return (is4xxClientError() || is5xxServerError());
		}

		/**
		 * Return a string representation of this status code.
		 */
		@Override
		public String toString() {
			return this.value + " " + name();
		}


		/**
		 * Return the {@code HttpStatus} enum constant with the specified numeric value.
		 */
		public static CustomHttpStatus valueOf(int statusCode) {
			CustomHttpStatus status = resolve(statusCode);
			if (status == null) {
				throw new IllegalArgumentException("No matching constant for [" + statusCode + "]");
			}
			return status;
		}

		/**
		 * Resolve the given status code to an {@code HttpStatus}, if possible.
		 */
		@Nullable
		public static CustomHttpStatus resolve(int statusCode) {
			// Use cached VALUES instead of values() to prevent array allocation.
			for (CustomHttpStatus status : VALUES) {
				if (status.value == statusCode) {
					return status;
				}
			}
			return null;
		}


		/**
		 * Enumeration of HTTP status series.
		 */
		public enum Series {

			INFORMATIONAL(1),
			SUCCESSFUL(2),
			REDIRECTION(3),
			CLIENT_ERROR(4),
			SERVER_ERROR(5);

			private final int value;

			Series(int value) {
				this.value = value;
			}

			/**
			 * Return the integer value of this status series. Ranges from 1 to 5.
			 */
			public int value() {
				return this.value;
			}

			/**
			 * Return the {@code Series} enum constant for the supplied {@code HttpStatus}.
			 */
			@Deprecated
			public static Series valueOf(CustomHttpStatus status) {
				return status.series;
			}

			/**
			 * Return the {@code Series} enum constant for the supplied status code.
			 */
			public static Series valueOf(int statusCode) {
				Series series = resolve(statusCode);
				if (series == null) {
					throw new IllegalArgumentException("No matching constant for [" + statusCode + "]");
				}
				return series;
			}

			/**
			 * Resolve the given status code to an {@code HttpStatus.Series}, if possible.
			 */
			@Nullable
			public static Series resolve(int statusCode) {
				int seriesCode = statusCode / 100;
				for (Series series : values()) {
					if (series.value == seriesCode) {
						return series;
					}
				}
				return null;
			}
		}

}
