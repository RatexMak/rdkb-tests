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
package com.automatics.rdkb.tests.telemetry;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

/**
 * Class for Validating the Parodus logs as per WebPA Telemetry data.
 */

public class ParodusWebpaTelemetryTest extends AutomaticsTestBase {

    /**
     *
     * Test Case # 1: Verify the logging of WebPA telemetry data in the Parodus Logs.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify there is no WebPA Process Crash.</li>
     * <li>S2) Verify killing the WebPA process logs message that WebPA process is not running and it would be
     * restarted.</li>
     * <li>S3) Verify there is no PARODUS Process Crash.</li>
     * <li>S4) Verify killing the PARODUS process logs message that PARODUS process is not running and it would be
     * restarted.</li>
     * <li>S5) Verify setting the WebPA parameter with appropriate value does NOT log error message that parameter value
     * field is not available.</li>
     * <li>S6)Verify server is connected over SSL in PARODUS log file.</li>
     * <li>S7)Verify the parodus is enabled in device</li>
     * <li>S8) Verify the logging of device reboot reason.</li>
     * <li>S9) Verify setting the WebPA parameter without the value field logs error message that parameter value field
     * is not available.</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Govardhan
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-5515")

    public void testParodusWebPaTelemetry(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-515";
	String response = null;
	String errorMessage = null;
	String step = "s1";
	boolean result = false;
	boolean isAtomConsoleAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5515");
	LOGGER.info("TEST DESCRIPTION: Verify the logging of WebPA telemetry data in the Parodus Logs");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(" 1: Verify there is no WebPA Process Crash");
	LOGGER.info(
		" 2: Verify killing the WebPA process logs message that WebPA process is not running and it would be restarted");
	LOGGER.info(" 3: Verify there is no PARODUS Process Crash");
	LOGGER.info(
		" 4: Verify killing the PARODUS process logs message that PARODUS process is not running and it would be restarted");
	LOGGER.info(
		" 5: Verify setting the WebPA parameter with appropriate value does NOT log error message that parameter value field is not available");
	LOGGER.info(" 6: Verify server is connected over SSL in PARODUS log file");
	LOGGER.info(" 7: Verify the parodus is enabled in device");
	LOGGER.info(" 8: Verify the logging of device reboot reason.");
	LOGGER.info(
		" 9: Verify setting the WebPA parameter without the value field logs error message that parameter value field is not available.");
	LOGGER.info("#######################################################################################");
	try {
	    /**
	     * S1) Verify there is no WebPA Process Crash.
	     */
	    errorMessage = "WEBPA PROCESS HAD CRASHED EARLIER.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : VERIFY THERE IS NO CRASH OF WEBPA PROCESS.");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the command in ATOM console:grep -i \"WebPA_process is not running, restarting it\" /rdklogs/logs/AtomConsolelog.txt.0");
	    LOGGER.info(
		    "STEP 1: EXPECTED : LOG MESSAGE 'WEBPA_PROCESS IS NOT RUNNING, RESTARTING IT' MUST NOT BE PRESENT.");
	    LOGGER.info("**********************************************************************************");

	    if (isAtomConsoleAvailable) {
		response = BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_WEBPA_PROCESS_RESTART,
			BroadBandCommandConstants.LOG_FILE_ATOM_CONSOLE);
		result = CommonMethods.isNull(response);
		if (result) {
		    LOGGER.info("STEP 1: ACTUAL : NO INSTANCE OF WEBPA PROCESS CRASH TRACED.");
		} else {
		    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		LOGGER.info("STEP 1 is applicable only for  devices which have Atom Console..Not applicable for  "
			+ device.getModel() + "  Model !!!");

		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			"This step is applicable only for devices which have Atom Console", false);
	    }

	    /**
	     * S2) Verify killing the WebPA process logs message that WebPA process is not running and it would be
	     * restarted.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : VERIFY KILLING THE WEBPA PROCESS LOGS MESSAGE 'WebPA_process is not running, restarting it'.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the command in ATOM console:grep -i \"WebPA_process is not running, restarting it\" /rdklogs/logs/AtomConsolelog.txt.0");
	    LOGGER.info(
		    "STEP 2: EXPECTED : LOG MESSAGE 'WebPA_process is not running, restarting it' MUST BE PRESENT.");
	    LOGGER.info("**********************************************************************************");
	    
	    String deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    // Kill the WebPA Process.
	    result = BroadBandCommonUtils.killProcessAndVerifyInAtomConsole(tapEnv, device,
		    BroadBandTestConstants.PROCESS_NAME_WEBPA);
	    errorMessage = "KILLING WEBPA PROCESS FAILED";
	    // Validation of the WebPA Process starts within 15 Minutes.
	    long pollDuration = BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS;
	    long startTime = System.currentTimeMillis();
	    int loopCounter = 1;
	    if (result) {
		do {
		    LOGGER.info("WEBPA PROCESS ID VALIDATION: " + loopCounter++);
		    LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
		    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		    response = BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
			    BroadBandTestConstants.PROCESS_NAME_WEBPA);
		    result = CommonMethods.isNotNull(response);
		} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
		errorMessage = "RESTART OF WEBPA PROCESS FAILED.";
	    }
	    // Validation of the presence of the log message.
	    if (result) {
		result = false;
		LOGGER.info("WEBPA PROCESS RESTARTED WITH PID: " + response);
		pollDuration = BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS;
		startTime = System.currentTimeMillis();
		loopCounter = 1;
		do {
		    LOGGER.info("WEBPA RESTART LOG VALIDATION: " + loopCounter++);
		    LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
		    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		    result = BroadBandSystemUtils.verifyAtomConsoleLog(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_WEBPA_PROCESS_RESTART,
			    BroadBandCommandConstants.LOG_FILE_ATOM_CONSOLE, deviceDateTime);
		} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);	
		errorMessage = "UNABLE TO VERIFY THE LOG MESSAGE 'WebPA_process is not running, restarting it' ON KILLING WEBPA PROCESS.";
	    }
	    if (!result) {
		LOGGER.info("VALIDATING WEBPA RESTART LOG IN SETTOP TRACE");
		try {
		    result = CommonUtils.validateTraceLog(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_WEBPA_PROCESS_RESTART,
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);		    
		} catch (Exception e) {
		    LOGGER.error("Exception occured while validating settop trace " + e.getMessage());
		}
	    }
	    if (result) {
		LOGGER.info(
			"STEP 2: ACTUAL : KILLING WEBPA PROCESS LOGGED MESSAGE: 'WebPA_process is not running, restarting it'.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);


	    /**
	     * S3) Verify there is no Parodus Process Crash.
	     */
	    step = "s3";
	    result = false;
	    errorMessage = "PARODUS PROCESS HAD CRASHED EARLIER.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : VERIFY THERE IS NO CRASH OF PARODUS PROCESS.");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the command in ARM console:grep -i \"parodus process is not running, need restart\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info(
		    "STEP 3: EXPECTED : LOG MESSAGE 'parodus process is not running, need restart' MUST NOT BE PRESENT.");
	    LOGGER.info("**********************************************************************************");
	    StringBuilder sbCommand = new StringBuilder(BroadBandTestConstants.GREP_COMMAND);
	    sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_PARODUS_PROCESS_RESTART);
	    sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
	    sbCommand.append(BroadBandCommandConstants.LOG_FILE_SELFHEAL);
	    LOGGER.info("COMMAND TO BE EXECUTED: " + sbCommand.toString());
	    result = !CommonUtils.searchLogFiles(tapEnv, device, sbCommand.toString());
	    if (result) {
		LOGGER.info("STEP 3: ACTUAL : NO INSTANCE OF PARODUS PROCESS CRASH TRACED.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S4) Verify killing the PARODUS process logs message that PARODUS process is not running and it would be
	     * restarted.
	     */
	    step = "s4";
	    result = false;
	    errorMessage = "KILLING PARODUS PROCESS FAILED";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : VERIFY KILLING PARODUS PROCESS LOGS MESSAGE 'parodus process is not running, need restart'");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the command in ARM console:grep -i \"parodus process is not running, need restart\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info(
		    "STEP 4: EXPECTED : LOG MESSAGE 'parodus process is not running, need restart' MUST BE PRESENT.");
	    LOGGER.info("**********************************************************************************");

		// Kill the Parodus Process
		result = CommonUtils.getProcessIdAndKill(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS);
		// Verify the Parodus Process starts within 10 minutes.
		if (result) {
		    pollDuration = BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS;
		    startTime = System.currentTimeMillis();
		    loopCounter = 1;
		    do {
			LOGGER.info("PARODUS PROCESS ID VALIDATION: " + loopCounter++);
			LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			response = CommonUtils.getPidOfProcess(device, tapEnv,
				BroadBandTestConstants.PROCESS_NAME_PARODUS);
			result = CommonMethods.isNotNull(response);
		    } while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
		    errorMessage = "RESTART OF PARODUS PROCESS FAILED.";
		}
		// Validation of the presence of log message.
		if (result) {
		    result = false;
		    LOGGER.info("PARODUS PROCESS RESTARTED WITH PID: " + response);
		    sbCommand = new StringBuilder(BroadBandTestConstants.GREP_COMMAND);
		    sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_PARODUS_PROCESS_RESTART);
		    sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
		    sbCommand.append(BroadBandCommandConstants.LOG_FILE_SELFHEAL);
		    LOGGER.info("COMMAND TO BE EXECUTED: " + sbCommand.toString());
		    pollDuration = BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS;
		    startTime = System.currentTimeMillis();
		    loopCounter = 1;
		    do {
			LOGGER.info("PARODUS PROCESS RESTART LOG VALIDATION: " + loopCounter++);
			LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
			tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			result = CommonUtils.searchLogFiles(tapEnv, device, sbCommand.toString());
		    } while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
		    errorMessage = "UNABLE TO VERIFY THE LOG MESSAGE 'parodus process is not running, need restart' ON KILLING THE PARODUS PROCESS";
		}
		if (!result) {
		    LOGGER.info("VALIDATING PARODUS PROCESS RESTART LOG IN DEVICE TRACE");
		    try {
			result = CommonUtils.validateTraceLog(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_PARODUS_PROCESS_RESTART,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		    } catch (Exception e) {
			LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		    }
		}
		if (result) {
		    LOGGER.info(
			    "STEP 4: ACTUAL : LOG MESSAGE 'parodus process is not running, need restart' IS PRESENT ON KILLING PARODUS PROCESS.");
		} else {
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S5) Verify setting the WebPA parameter with appropriate value does NOT log error message that parameter
	     * value field is not available.
	     */
	    step = "s5";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : VERIFY SETTING THE WEBPA PARAMETER WITH APPROPRIATE VALUE DOES NOT LOG ERROR MESSAGE.'");
	    LOGGER.info(
		    "STEP 5: ACTION : After Doing WebPA SET ,Execute the command in ATOM console:grep -i \"Parameter value field is not available\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 5: EXPECTED : LOG MESSAGE 'Parameter value field is not available' MUST NOT BE PRESENT.");
	    LOGGER.info("**********************************************************************************");
	    WebPaParameter webPaParam = new WebPaParameter();
	    webPaParam.setDataType(RDKBTestConstants.CONSTANT_3);
	    webPaParam.setName(BroadBandWebPaConstants.WEBPA_PARAM_Device_WiFi_SSID_10101_Enable);
	    webPaParam.setValue(RDKBTestConstants.TRUE);
	    errorMessage = "UNABLE TO SET THE WEBPA PARAMETERS WITH VALUE FIELD";
	    if (BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParam)) {
		LOGGER.info("GOING TO WAIT FOR ONE MINUTE.");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		response = BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_WEBPA_WITHOUT_FIELD,
			BroadBandCommandConstants.LOG_FILE_WEBPA);
	    }
	    result = CommonMethods.isNull(response)
		    || !response.contains(BroadBandTraceConstants.LOG_MESSAGE_WEBPA_WITHOUT_FIELD
			    .replaceAll(BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
	    errorMessage = "ERROR-LOG MESSAGE PRESENT ON SETTING WEBPA PARAMETER WITH APPROPRIATE VALUE.";
	    if (result) {
		LOGGER.info("STEP 5: ACTUAL : LOG MESSAGE 'Parameter value field is not available' IS NOT BE PRESENT.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    LOGGER.info(
		    "### PRE-CONDITION ### GOING TO REBOOT THE DEVICE TO VERIFY REBOOT REASON & PARODUS STATUS LOGS.");
	    if (!CommonUtils.rebootUsingWebpaAndWaitForIpAcquisition(tapEnv, device)) {
		errorMessage = "Unable to reboot the device successfully.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    /**
	     * S6)Verify server is connected over SSL in PARODUS log file
	     * 
	     */
	    step = "s6";
	    result = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 6:DESCRIPTION :Verify server is connected over SSL in PARODUS log file");
	    LOGGER.info(
		    "STEP 6: ACTION : Search for 'Connected to server over SSL' log message in /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info("STEP 6:EXPECTED: Log message should be present in PARODUSlog.txt");
	    LOGGER.info("***********************************************************");
	    result = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, BroadBandCommandConstants.LOG_FILE_PARODUS,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    errorMessage = CommonUtils.concatStringUsingStringBuffer(
		    "Unable to verify server connection from log file:", BroadBandCommandConstants.LOG_FILE_PARODUS,
		    "with log message", BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION);
	    if (result) {
		LOGGER.info("STEP 6: ACTUAL : Server Connection is verified successfully from log file:"
			+ BroadBandCommandConstants.LOG_FILE_PARODUS);
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S7)Verify the parodus is enabled in device
	     * 
	     */
	    step = "s7";
	    result = false;
	    errorMessage = "Unable to get the status of Parodus using command:"
		    + BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify parodus process is running on ARM side.");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute the below command in ARM console: Command: root@Docsis-Gateway:/rdklogs/logs # ps | grep parodus ");
	    LOGGER.info("STEP 7: EXPECTED : parodus process details should be displayed as (/usr/bin/parodus). ");
	    LOGGER.info("**********************************************************************************");
	    result = CommonMethods.isNotNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS));
	    if (result) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verified the status of parodus using command:"
			+ BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS);
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S8) Verify the logging of device reboot reason.
	     */
	    step = "s8";
	    result = false;
	    errorMessage = "UNABLE TO VERIFY THE LOG MESSAGE 'Received reboot_reason as:webpa-reboot' ON DEVICE STARTUP.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : VERIFY THE LOGGING OF DEVICE REBOOT REASON");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command in ARM console: grep -i \"Received reboot_reason as:webpa-reboot\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info(
		    "STEP 8: EXPECTED : LOG MESSAGE 'Received reboot_reason as:webpa-reboot' MUST BE PRESENT ON DEVICE STARTUP.");
	    LOGGER.info("**********************************************************************************");

		sbCommand = new StringBuilder(BroadBandTestConstants.GREP_COMMAND);
		sbCommand.append(BroadBandTraceConstants.LOG_MESSAGE_RECEIVED_REBOOT_REASON);
		sbCommand.append(BroadBandTestConstants.SINGLE_SPACE_CHARACTER);
		sbCommand.append(BroadBandCommandConstants.LOG_FILE_PARODUS);
		LOGGER.info("COMMAND TO BE EXECUTED: " + sbCommand.toString());
		pollDuration = BroadBandTestConstants.TEN_MINUTE_IN_MILLIS;
		startTime = System.currentTimeMillis();
		loopCounter = 1;
		do {
		    LOGGER.info("DEVICE REBOOT REASON LOG VALIDATION: " + loopCounter++);
		    LOGGER.info("GOING TO WAIT FOR ONE MINUTE.");
		    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		    result = CommonUtils.searchLogFiles(tapEnv, device, sbCommand.toString());
		} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
		if (!result) {
		    LOGGER.info("VALIDATING DEVICE REBOOT REASON LOG IN DEVICE TRACE");
		    try {
			result = CommonUtils.validateTraceLog(tapEnv, device,
				BroadBandTraceConstants.LOG_MESSAGE_RECEIVED_REBOOT_REASON,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		    } catch (Exception e) {
			LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		    }
		}
		if (result) {
		    LOGGER.info(
			    "STEP 8: ACTUAL : LOG MESSAGE 'Received reboot_reason as:webpa-reboot' IS PRESENT ON DEVICE STARTUP.");
		} else {
		    LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /**
	     * S9) Verify setting the WebPA parameter without the value field logs error message that parameter value
	     * field is not available.
	     */
	    step = "s9";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : VERIFY SETTING THE WEBPA PARAMETER WITHOUT VALUE FIELD LOGS ERROR MESSAGE 'Parameter value field is not available'.");
	    LOGGER.info(
		    "STEP 9: ACTION : After Doing WebPA Set,Execute the command: grep -i \"Parameter value field is not available\" /rdklogs/logs/WEBPAlog.txt.0");
	    LOGGER.info("STEP 9: EXPECTED :  LOG MESSAGE 'Parameter value field is not available' MUST BE PRESENT.");
	    LOGGER.info("**********************************************************************************");
	    webPaParam = new WebPaParameter();
	    webPaParam.setDataType(RDKBTestConstants.CONSTANT_0);
	    webPaParam.setName(
		    BroadBandWebPaConstants.WEBPA_PARAM_Device_DeviceInfo_X_RDKCENTRAL_COM_FirmwareDownloadProtocol);
	    result = BroadBandCommonUtils.setWebPaParam(tapEnv, device, webPaParam);
	    errorMessage = "SETTING WEBPA PARAM WITHOUT VALUE FIELD RETURNS SUCCESS.";
	    if (!result) {
		LOGGER.info("GOING TO WAIT FOR 1 MINUTE.");
		pollDuration = BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS;
		startTime = System.currentTimeMillis();
		loopCounter = 1;
		do {
		    LOGGER.info("WEBPA PARAMETER VALUE NOT AVAILABLE LOG VALIDATION: " + loopCounter++);
		    LOGGER.info("GOING TO WAIT FOR ONE MINUTE.");
		    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		    response = BroadBandCommonUtils.searchAtomConsoleLogs(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_WEBPA_WITHOUT_FIELD,
			    BroadBandCommandConstants.LOG_FILE_WEBPA);
		    result = CommonMethods.isNotNull(response)
			    && response.contains(BroadBandTraceConstants.LOG_MESSAGE_WEBPA_WITHOUT_FIELD.replaceAll(
				    BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING));
		} while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
		errorMessage = "UNABLE TO VERIFY THE LOG MESSAGE 'Parameter value field is not available'.";
	    }
	    if (!result) {
		LOGGER.info("VALIDATING WEBPA PARAMETER VALUE NOT AVAILABLE IN DEVICE TRACE");
		try {
		    result = CommonUtils.validateTraceLog(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_WEBPA_WITHOUT_FIELD,
			    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, true);
		} catch (Exception e) {
		    LOGGER.error("Exception occured while validating device trace " + e.getMessage());
		}
	    }
	    if (result) {
		LOGGER.info(
			"STEP 9: ACTUAL : LOG MESSAGE 'Parameter value field is not available' IS PRESENT ON TRYING TO SET WEBPA PARAMETER WITHOUT VALUE FIELD.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE PARODUS LOGS AS PER WEBPA TELEMETRY DATA: "
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	}
    }
}