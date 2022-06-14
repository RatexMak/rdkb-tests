/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.harvester;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.LoggerUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

public class HarvesterLoggerTest extends AutomaticsTestBase {

	/**
	 *
	 * Verify the integration of RDK-B Logger with Harvester.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>S1) Verify the file /rdklogs/logs/Harvesterlog.txt.0 is present in ATOM
	 * Console.</li>
	 * <li>S2) Verify the successful initialization of rdk-logger.</li>
	 * <li>S3) Verify enabling Radio Interface Statistics and confirm the harvester
	 * log in ATOM Console is updated.</li>
	 * <li>S4) Verify enabling Interface Devices Wifi Report and confirm the
	 * harvester log in ATOM Console is updated.</li>
	 * </ol>
	 *
	 * @author BALAJI V
	 * @refactor Athira
	 * 
	 * @param device {@link Dut}
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.HARVESTER })

	@TestDetails(testUID = "TC-RDKB-RDKLOG-HARV-INTG-5003")
	public void testHarvesterLog(Dut device) {
		String testCaseId = "TC-RDKB-RDKLOG-HARV-INTG-503";
		boolean interfaceDevicesWifiReportEnabled = false;
		boolean radioInterfaceStatisticsEnabled = false;
		String command = null;
		boolean result = false;
		String errorMessage = null;
		String step = "s1";
		try {
			LOGGER.info("################### STARTING TEST CASE TC-RDKB-RDKLOG-HARV-INTG-5003 ###################");
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
			LOGGER.info("### PRE-CONDITION ### GOING TO REBOOT THE DEVICE.");
			if (!CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
				errorMessage = "Unable to reboot the device successfully.";
				LOGGER.error(errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}

			/**
			 * S1) Verify the file /rdklogs/logs/Harvesterlog.txt.0 is present in ATOM
			 * Console.
			 */
			LOGGER.info("S1) VERIFY HARVESTER LOG IS PRESENT IN ATOM CONSOLE.");
			LOGGER.info("S1 - EXPECTED - HARVESTER LOG FILE MUST BE PRESENT.");
			long pollDuration = BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS;
			long startTime = System.currentTimeMillis();
			do {
				LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				result = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_HARVESTER_LOG);
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
			errorMessage = "HARVESTER LOG IS NOT PRESENT IN ATOM CONSOLE.";
			LOGGER.info("S1 - ACTUAL: " + (result ? "HARVESTER LOG IS PRESENT IN ATOM CONSOLE." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);
			/**
			 * S2) Verify the successful initialization of rdk-logger.
			 */
			step = "s2";
			result = false;
			LOGGER.info("S2) VERIFY THE SUCCESSFUL INITIALIZATION OF RDK LOGGER.");
			LOGGER.info("S2 - EXPECTED - RDK LOGGER MUST BE INITIALIZED SUCCESSFULLY.");
			pollDuration = BroadBandTestConstants.FIVE_MINUTES;
			command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
					BroadBandTraceConstants.LOG_MESSAGE_RDK_LOGGER_INITIALIZED,
					BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_HARVESTER_LOG);
			startTime = System.currentTimeMillis();
			do {
				LOGGER.info("GOING TO WAIT FOR 30 SECONDS.");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				result = CommonUtils.searchLogFiles(tapEnv, device, command);
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
			errorMessage = "UNABLE TO VERIFY RDK LOGGER INITIALIZATION.";
			LOGGER.info("S2 - ACTUAL: " + (result ? "RDK LOGGER IS INITIALIZED SUCCESSFULLY." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S3) Verify enabling Radio Interface Statistics and confirm the harvester log
			 * in ATOM Console is updated.
			 */
			step = "s3";
			result = false;
			LOGGER.info("S3) VERIFY ENABLING THE RADIO INTERFACE STATISTICS.");
			LOGGER.info("S3 - EXPECTED - RADIO INTERFACE STATISTICS MUST BE ENABLED & LOGS MUST BE PRESENT.");
			WebPaParameter webPaParameter = new WebPaParameter();
			webPaParameter.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			webPaParameter.setValue("true");
			webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			radioInterfaceStatisticsEnabled = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
			errorMessage = "UNABLE TO ENABLE THE RADIO INTERFACE STATISTICS.";
			// Changing the Search File from /rdklogs/logs/Harvesterlog.txt.0 to
			// /rdklogs/logs/WEBPAlog.txt.0
			if (radioInterfaceStatisticsEnabled) {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
						BroadBandTraceConstants.LOG_MESSAGE_RADIO_INTERFACE_STATISTICS_ENABLED,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.LOG_FILE_WEBPA);
				startTime = System.currentTimeMillis();
				do {
					LOGGER.info("GOING TO WAIT FOR 30 SECONDS.");
					tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					result = CommonUtils.searchLogFiles(tapEnv, device, command);
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS
						&& !result);
				errorMessage = "UNABLE TO VERIFY HARVESTER LOGS (ATOM CONSOLE) FOR ENABLING RADIO INTERFACE STATISTICS.";
			}
			LOGGER.info("S3 - ACTUAL: "
					+ (result ? "ATOM CONSOLE LOG UPDATED ON ENABLING RADIO INTERFACE STATISTICS." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S4) Verify enabling Interface Devices Wifi Report and confirm the harvester
			 * log in ATOM Console is updated.
			 */
			step = "s4";
			result = false;
			LOGGER.info("S4) VERIFY ENABLING THE INTERFACE DEVICES WIFI REPORT.");
			LOGGER.info("S4 - EXPECTED - INTERFACE DEVICES WIFI REPORT MUST BE ENABLED & LOGS MUST BE PRESENT.");
			webPaParameter = new WebPaParameter();
			webPaParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			webPaParameter.setValue("true");
			webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			interfaceDevicesWifiReportEnabled = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
			errorMessage = "UNABLE TO ENABLE THE INTERFACE DEVICES WIFI REPORT.";
			if (interfaceDevicesWifiReportEnabled) {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
						BroadBandTraceConstants.LOG_MESSAGE_INTERFACE_DEVICE_WIFI_REPORT_ENABLED,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_HARVESTER_LOG);
				startTime = System.currentTimeMillis();
				do {
					LOGGER.info("GOING TO WAIT FOR 30 SECONDS.");
					tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					result = CommonUtils.searchLogFiles(tapEnv, device, command);
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS
						&& !result);
				errorMessage = "UNABLE TO VERIFY HARVESTER LOGS (ATOM CONSOLE) FOR ENABLING INTERFACE WIFI DEVICES REPORT.";
			}
			LOGGER.info("S4 - ACTUAL: "
					+ (result ? "ATOM CONSOLE LOG UPDATED ON ENABLING INTERFACE DEVICES WIFI REPORT." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE INTEGRATION OF RDK LOGGER WITH HARVESTER: "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
		} finally {
			// Disable the Interface Devices Wifi Report, in case it is Enabled earlier.
			if (interfaceDevicesWifiReportEnabled) {
				LOGGER.info("### POST-CONDITION ### DISABLING THE INTERFACE DEVICES WIFI REPORT");
				WebPaParameter webPaParameter = new WebPaParameter();
				webPaParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
				webPaParameter.setValue("false");
				webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
				result = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
				LOGGER.info("### POST-CONDITION ### DISABLED THE INTERFACE DEVICES WIFI REPORT: " + result);
			}
			// Disable the Radio Interface Statistics, in case it is Enabled earlier.
			if (radioInterfaceStatisticsEnabled) {
				LOGGER.info("### POST-CONDITION ### DISABLING THE RADIO INTERFACE STATISTICS");
				WebPaParameter webPaParameter = new WebPaParameter();
				webPaParameter.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
				webPaParameter.setValue("false");
				webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
				result = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
				LOGGER.info("### POST-CONDITION ### DISABLED THE RADIO INTERFACE STATISTICS: " + result);
			}
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
			LOGGER.info("################### ENDING TEST CASE TC-RDKB-RDKLOG-HARV-INTG-5003 ###################");
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
		}
	}

	/**
	 *
	 * Test Case # 1: Verify the integration of RDK-B Logger with Harvester.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>S1) Verify the file /rdklogs/logs/Harvesterlog.txt.0 is present in ARM
	 * Console.</li>
	 * <li>S2) Verify the file /rdklogs/logs/Harvesterlog.txt.0 is present in ATOM
	 * Console.</li>
	 * <li>S3) Verify the successful initialization of rdk-logger.</li>
	 * <li>S4) Verify the harvester log in ATOM & ARM Consoles are in sync.</li>
	 * <li>S5) Verify enabling Radio Interface Statistics and confirm the harvester
	 * log in ATOM Console is updated.</li>
	 * <li>S6) Verify enabling Interface Devices Wifi Report and confirm the
	 * harvester log in ATOM Console is updated.</li>
	 * <li>S7) Verify the harvester log in ARM Console is updated for enabling Radio
	 * Interface Statistics.</li>
	 * <li>S8) Verify the harvester log in ARM Console is updated for enabling
	 * Interface Devices Wifi Report.</li>
	 * </ol>
	 *
	 * @author BALAJI V
	 * 
	 * @param device {@link Dut}
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
			BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.HARVESTER })

	@TestDetails(testUID = "TC-RDKB-HARV-5001")
	public void testHarvesterLogSync(Dut device) {
		String testCaseId = "TC-RDKB-HARV-501";
		boolean interfaceDevicesWifiReportEnabled = false;
		boolean radioInterfaceStatisticsEnabled = false;
		boolean result = false;
		String errorMessage = null;
		String step = "s1";
		try {
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
			LOGGER.info("TC-RDKB-HARV-5001- AUTOMATION TEST COVERS THE RDK LOGGER INTEGRATION WITH HARVESTER.");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-HARV-5001");
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
			LOGGER.info("### PRE-CONDITION ### GOING TO REBOOT THE DEVICE.");
			if (!CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
				errorMessage = "Unable to reboot the device successfully.";
				LOGGER.error(errorMessage);
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}

			/**
			 * S1) Verify the file /rdklogs/logs/Harvesterlog.txt.0 is present in ATOM
			 * Console.
			 */
			LOGGER.info("S1) VERIFY HARVESTER LOG IS PRESENT IN ATOM CONSOLE.");
			LOGGER.info("S1 - EXPECTED - HARVESTER LOG FILE MUST BE PRESENT.");
			long pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
			long startTime = System.currentTimeMillis();
			do {
				LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				BroadBandResultObject objResult = BroadBandCommonUtils.doesFileExistInAtomConsole(device, tapEnv,
						BroadBandCommandConstants.FILE_HARVESTER_LOG);
				if (null != objResult) {
					result = objResult.isStatus();
					errorMessage = objResult.getErrorMessage();
				}
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
			LOGGER.info("S1 - ACTUAL: " + (result ? "HARVESTER LOG IS PRESENT IN ATOM CONSOLE." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S2) Verify the successful initialization of rdk-logger.
			 */
			step = "s2";
			result = false;
			LOGGER.info("S2) VERIFY THE SUCCESSFUL INITIALIZATION OF RDK LOGGER.");
			LOGGER.info("S2 - EXPECTED - RDK LOGGER MUST BE INITIALIZED SUCCESSFULLY.");
			String response = null;
			pollDuration = BroadBandTestConstants.FIVE_MINUTES;
			startTime = System.currentTimeMillis();
			do {
				LOGGER.info("GOING TO WAIT FOR 30 SECONDS.");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				response = BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_RDK_LOGGER_INITIALIZED,
						BroadBandCommandConstants.FILE_HARVESTER_LOG);
				result = CommonMethods.isNotNull(response);
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
			errorMessage = "UNABLE TO VERIFY RDK LOGGER INITIALIZATION.";
			// Capturing the ATOM Console Log Content earlier to avoid roll over.
			String atomConsoleHarvesterLog = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
							BroadBandCommandConstants.FILE_HARVESTER_LOG));
			atomConsoleHarvesterLog = CommonMethods.isNotNull(atomConsoleHarvesterLog) ? atomConsoleHarvesterLog.trim()
					: null;
			LOGGER.info("S2 - ACTUAL: " + (result ? "RDK LOGGER IS INITIALIZED SUCCESSFULLY." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S3) Verify the file /rdklogs/logs/Harvesterlog.txt.0 is present in ARM
			 * Console.
			 */
			step = "s3";
			result = false;
			LOGGER.info("S3) VERIFY HARVESTER LOG IS PRESENT IN ARM CONSOLE.");
			LOGGER.info("S3 - EXPECTED - HARVESTER LOG FILE MUST BE PRESENT.");
			pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
			startTime = System.currentTimeMillis();
			do {
				LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				result = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_HARVESTER_LOG);
			} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
			errorMessage = "HARVESTER LOG IS NOT PRESENT IN ARM CONSOLE.";
			LOGGER.info("S3 - ACTUAL: " + (result ? "HARVESTER LOG IS PRESENT IN ARM CONSOLE." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

			/**
			 * S4) Verify the harvester log in ATOM & ARM Consoles are in sync.
			 */
			step = "s4";
			result = false;
			LOGGER.info("S4) VERIFY THE HARVESTER LOG IN ATOM & ARM CONSOLES ARE IN SYNC.");
			LOGGER.info("S4 - EXPECTED - THE HARVESTER LOG IN ATOM & ARM CONSOLES MUST BE IN SYNC.");
			String armConsoleHarvesterLog = tapEnv.executeCommandUsingSsh(device,
					BroadBandTestConstants.CAT_COMMAND + BroadBandCommandConstants.FILE_HARVESTER_LOG);
			result = LoggerUtils.compareAtomArmConsoleLogs(atomConsoleHarvesterLog, armConsoleHarvesterLog);
			errorMessage = "HARVESTER LOG IN ATOM CONSOLE & ARM CONSOLE ARE NOT IN SYNC.";
			LOGGER.info(
					"S4 - ACTUAL: " + (result ? "HARVESTER LOG IN ATOM & ARM CONSOLES ARE IN SYNC." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S5) Verify enabling Radio Interface Statistics and confirm the harvester log
			 * in ATOM Console is updated.
			 */
			step = "s5";
			result = false;
			LOGGER.info("S5) VERIFY ENABLING THE RADIO INTERFACE STATISTICS.");
			LOGGER.info("S5 - EXPECTED - RADIO INTERFACE STATISTICS MUST BE ENABLED & LOGS MUST BE PRESENT.");
			WebPaParameter webPaParameter = new WebPaParameter();
			webPaParameter.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
			webPaParameter.setValue("true");
			webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			radioInterfaceStatisticsEnabled = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
			errorMessage = "UNABLE TO ENABLE THE RADIO INTERFACE STATISTICS.";
			String atomConsoleHarvesterLogRadioInterface = null;
			if (radioInterfaceStatisticsEnabled) {
				pollDuration = BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {
					LOGGER.info("GOING TO WAIT FOR 30 SECONDS.");
					tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					atomConsoleHarvesterLogRadioInterface = BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_RADIO_INTERFACE_STATISTICS_ENABLED,
							BroadBandCommandConstants.FILE_HARVESTER_LOG);
					result = CommonMethods.isNotNull(atomConsoleHarvesterLogRadioInterface);
				} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
				errorMessage = "UNABLE TO VERIFY HARVESTER LOGS (ATOM CONSOLE) FOR ENABLING RADIO INTERFACE STATISTICS.";
			}
			LOGGER.info("S5 - ACTUAL: "
					+ (result ? "ATOM CONSOLE LOG UPDATED ON ENABLING RADIO INTERFACE STATISTICS." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S6) Verify enabling Interface Devices Wifi Report and confirm the harvester
			 * log in ATOM Console is updated.
			 */
			step = "s6";
			result = false;
			LOGGER.info("S6) VERIFY ENABLING THE INTERFACE DEVICES WIFI REPORT.");
			LOGGER.info("S6 - EXPECTED - INTERFACE DEVICES WIFI REPORT MUST BE ENABLED & LOGS MUST BE PRESENT.");
			webPaParameter = new WebPaParameter();
			webPaParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
			webPaParameter.setValue("true");
			webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
			interfaceDevicesWifiReportEnabled = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
			errorMessage = "UNABLE TO ENABLE THE INTERFACE DEVICES WIFI REPORT.";
			String atomConsoleHarvesterLogInterfaceDevices = null;
			if (interfaceDevicesWifiReportEnabled) {
				LOGGER.info("GOING TO WAIT FOR 30 SECONDS.");
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				atomConsoleHarvesterLogInterfaceDevices = BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_INTERFACE_DEVICE_WIFI_REPORT_ENABLED,
						BroadBandCommandConstants.FILE_HARVESTER_LOG);
				result = CommonMethods.isNotNull(atomConsoleHarvesterLogInterfaceDevices);
				errorMessage = "UNABLE TO VERIFY HARVESTER LOGS (ATOM CONSOLE) FOR ENABLING INTERFACE WIFI DEVICES REPORT.";
			}
			LOGGER.info("S6 - ACTUAL: "
					+ (result ? "ATOM CONSOLE LOG UPDATED ON ENABLING INTERFACE DEVICES WIFI REPORT." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S7) Verify the harvester log in ARM Console is updated for enabling Radio
			 * Interface Statistics.
			 */
			step = "s7";
			result = false;
			LOGGER.info("S7) VERIFY THE HARVESTER LOG IN ARM CONSOLE FOR ENABLING RADIO INTERFACE STATISTICS.");
			LOGGER.info("S7 - EXPECTED - THE HARVESTER LOG MUST BE AVAILABLE IN ARM CONSOLE.");
			// Reason for using CAT Command and NOT GREP Command is the Sync Happens after
			// Step # 6 and it includes the logs for both the reports. By the time the grep
			// is successful and goes to the
			// next step, the logs are archived and step # s8 fails. Hence CAT is being used
			// and the response is
			// used for validating the next step as well.
			StringBuffer sbCommand = new StringBuffer(BroadBandTestConstants.CAT_COMMAND);
			sbCommand.append(BroadBandCommandConstants.FILE_HARVESTER_LOG);
			LOGGER.info("COMMAND TO BE EXECUTED: " + sbCommand.toString());
			pollDuration = BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS;
			startTime = System.currentTimeMillis();
			do {
				armConsoleHarvesterLog = tapEnv.executeCommandUsingSsh(device, sbCommand.toString());
				result = CommonMethods.isNotNull(armConsoleHarvesterLog) && armConsoleHarvesterLog
						.contains(BroadBandTraceConstants.LOG_MESSAGE_RADIO_INTERFACE_STATISTICS_ENABLED
								.replaceAll(BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
				// Reason for the less duration is logs are getting archived in short intervals.
				LOGGER.info("GOING TO WAIT FOR 20 SECONDS.");
				tapEnv.waitTill(BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			} while (CommonMethods.isNotNull(atomConsoleHarvesterLogRadioInterface)
					&& (System.currentTimeMillis() - startTime) < pollDuration && !result);
			errorMessage = CommonMethods.isNotNull(atomConsoleHarvesterLogRadioInterface)
					? "ARM CONSOLE HARVESTER LOG NOT SYNCED AFTER 10 MINUTES OF ENABLING RADIO INTERFACE STATISTICS."
					: "NO MESSAGES LOGGED IN ATOM CONSOLE ON ENABLING RADIO INTERFACE STATISTICS. HENCE FAILURE.";
			LOGGER.info("S7 - ACTUAL: "
					+ (result ? "ARM CONSOLE LOG UPDATED ON ENABLING RADIO INTERFACE STATISTICS." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

			/**
			 * S8) Verify the harvester log in ARM Console is updated for enabling Interface
			 * Devices Wifi Report.
			 */
			step = "s8";
			result = false;
			LOGGER.info("S8) VERIFY THE HARVESTER LOG IN ARM CONSOLE FOR ENABLING INTERFACE DEVICES WIFI REPORT.");
			LOGGER.info("S8 - EXPECTED - THE HARVESTER LOG MUST BE AVAILABLE IN ARM CONSOLE.");
			// Check whether the Sync had already happened & ARM Console Log has the
			// required message.
			result = CommonMethods.isNotNull(armConsoleHarvesterLog) && armConsoleHarvesterLog
					.contains(BroadBandTraceConstants.LOG_MESSAGE_INTERFACE_DEVICE_WIFI_REPORT_ENABLED
							.replaceAll(BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
			if (!result) {
				// In this case, the harvester log from ATOM & ARM Console cannot be validated
				// for IN SYNC, as the logs
				// would have been archived. Hence just the log messages are validated in ARM
				// Console.
				LOGGER.info(
						"ARM CONSOLE HARVESTER LOG FILE HAS NOT BEEN SYNCED WITH LOG MESSAGE FOR ENABLING WIFI REPORT YET.");
				sbCommand = new StringBuffer(BroadBandTestConstants.GREP_COMMAND);
				sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_INTERFACE_DEVICE_WIFI_REPORT_ENABLED);
				sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
				sbCommand.append(BroadBandCommandConstants.FILE_HARVESTER_LOG);
				LOGGER.info("COMMAND TO BE EXECUTED: " + sbCommand.toString());
				pollDuration = BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS;
				startTime = System.currentTimeMillis();
				do {
					armConsoleHarvesterLog = tapEnv.executeCommandUsingSsh(device, sbCommand.toString());
					result = CommonMethods.isNotNull(armConsoleHarvesterLog);
					// Reason for the less duration is logs are getting archived in short intervals.
					LOGGER.info("GOING TO WAIT FOR 20 SECONDS.");
					tapEnv.waitTill(BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
				} while (CommonMethods.isNotNull(atomConsoleHarvesterLogInterfaceDevices)
						&& (System.currentTimeMillis() - startTime) < pollDuration && !result);
				errorMessage = CommonMethods.isNotNull(atomConsoleHarvesterLogInterfaceDevices)
						? "ARM CONSOLE HARVESTER LOG NOT SYNCED AFTER 10 MINUTES OF ENABLING INTERFACE DEVICES WIFI REPORT."
						: "NO MESSAGES LOGGED IN ATOM CONSOLE ON ENABLING INTERFACE DEVICES WIFI REPORT. HENCE FAILURE.";

			}
			LOGGER.info("S8 - ACTUAL: "
					+ (result ? "ARM CONSOLE LOG UPDATED ON ENABLING INTERFACE DEVICES WIFI REPORT." : errorMessage));
			tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE INTEGRATION OF RDK LOGGER WITH HARVESTER: "
					+ errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
		} finally {
			// Disable the Interface Devices Wifi Report, in case it is Enabled earlier.
			if (interfaceDevicesWifiReportEnabled) {
				LOGGER.info("### POST-CONDITION ### DISABLING THE INTERFACE DEVICES WIFI REPORT");
				WebPaParameter webPaParameter = new WebPaParameter();
				webPaParameter.setName(BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT);
				webPaParameter.setValue("false");
				webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
				result = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
				LOGGER.info("### POST-CONDITION ### DISABLED THE INTERFACE DEVICES WIFI REPORT: " + result);
			}
			// Disable the Radio Interface Statistics, in case it is Enabled earlier.
			if (radioInterfaceStatisticsEnabled) {
				LOGGER.info("### POST-CONDITION ### DISABLING THE RADIO INTERFACE STATISTICS");
				WebPaParameter webPaParameter = new WebPaParameter();
				webPaParameter.setName(BroadBandWebPaConstants.WEBPA_RADIO_INTERFACE_STATISTICS);
				webPaParameter.setValue("false");
				webPaParameter.setDataType(BroadBandTestConstants.CONSTANT_3);
				result = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParameter);
				LOGGER.info("### POST-CONDITION ### DISABLED THE RADIO INTERFACE STATISTICS: " + result);
			}
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
			LOGGER.info(
					"TC-RDKB-HARV-5001/ TC-RDKB-HARV-501 - AUTOMATION TEST COVERS THE RDK LOGGER INTEGRATION WITH HARVESTER.");
			LOGGER.info("ENDING TEST CASE: TC-RDKB-HARV-5001");
			LOGGER.info(
					"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
		}
	}

}
