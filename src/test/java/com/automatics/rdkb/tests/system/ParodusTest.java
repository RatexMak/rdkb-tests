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
package com.automatics.rdkb.tests.system;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.CrashConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.enums.ProcessRestartOption;
import com.automatics.enums.StbProcess;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.TR69ParamConstants;
import com.automatics.rdkb.reboot.BootTime.BootTimePatterns;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.ParodusUtils;
import com.automatics.rdkb.utils.selfheal.BroadBandSelfHealUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

/**
 * 
 * Test class for validating Parodus Logs
 * 
 * @author Praveen_chandru
 * @refactor Govardhan
 */
public class ParodusTest extends AutomaticsTestBase {
	/**
	 * Verify parodus and webpa uptimes in bootup sequence
	 * <ol>
	 * <li>Step 1 : Obtain wan_init_complete time in ArmConsolelog for Atom Sync
	 * Available devices and Consolelog for other platforms</li>
	 * <li>Step 2 : Verify parodus process is running</li>
	 * <li>Step 3 : Obtain parodus starting time from timestamp in log message in
	 * PARODUSlog</li>
	 * <li>Step 4 : Verify parodus is connected to server over SSL log message in
	 * PARODUSlog</li>
	 * <li>Step 5 : Obtain parodus connected time from log message in
	 * PARODUSlog</li>
	 * <li>Step 6 : Obtain WebPA starting time from timestamp in log message in
	 * WEBPAlog</li>
	 * <li>Step 7 : Obtain boot_to_WEBPA_READY_uptime value from log message in
	 * WEBPAlog</li>
	 * </ol>
	 * 
	 * @author Ashwin sankara
	 * @refactor Govardhan
	 * 
	 * @param device {@link Dut}
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PARODUS-1005")
	public void testVerifyParodusWebpaUptimes(Dut device) {

		// Variable Declaration begins
		// stores the test case id
		String testCaseId = "TC-RDKB-PARODUS-105";
		// stores the step number

		int stepNumber = 1;
		String stepNum = "S" + stepNumber;
		// stores the error message
		String errorMessage = null;
		// stores the command response
		String response = null;
		// stores the boot time parameter value
		long bootTime = BroadBandTestConstants.CONSTANT_0;
		// stores the start time
		long startTime = BroadBandTestConstants.CONSTANT_0;
		// stores the expected time
		long expectTime = BroadBandTestConstants.CONSTANT_0;
		// stores the step result
		boolean status = false;
		// stores the success message
		String successMsg = null;
		// Variable Declation Ends
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1005");
		LOGGER.info("TEST DESCRIPTION: Verify parodus and webpa uptimes in bootup sequence");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("Step 1. Obtain Waninit_complete time in Boottime.log");
		LOGGER.info("Step 2. Obtain boot_to_WEBPA_READY_uptime value from log message in BootTime.log");
		LOGGER.info("Step 3. Verify parodus process is running");
		LOGGER.info("Step 4. Obtain parodus starting time from timestamp in log message in PARODUSlog");
		LOGGER.info("Step 5. Verify parodus is connected to server over SSL log message in PARODUSlog");
		LOGGER.info("Step 6. Obtain parodus connected time from log message in PARODUSlog");
		LOGGER.info("Step 7. Obtain WebPA starting time from timestamp in log message in WEBPAlog");
		LOGGER.info("Step 8.Verify the device manufacturer name using WebPA");
		LOGGER.info("Step 9.Verify the device manufacturer name in Parodus log file");

		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 1: DESCRIPTION : Reboot the device");
			LOGGER.info("PRE-CONDITION 1: ACTION : Execute reboot command on the console");
			LOGGER.info("PRE-CONDITION 1: EXPECTED : Device is rebooted successfully");
			LOGGER.info("#######################################################################################");
			status = BroadBandCommonUtils.rebootAndWaitForStbAccessible(device, tapEnv);
			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Device is rebooted successfully");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : Device is not rebooted");
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			/**
			 * STEP 1 : OBTAIN WAN_INIT_COMPLETE TIME IN ARMCONSOLELOG FOR ATOM SYNC
			 * AVAILABLE DEVICES AND CONSOLELOG FOR OTHER PLATFORMS
			 */
			stepNum = "s1";
			errorMessage = "Failed to obtain wan_init_complete time after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Obtain wan_init_complete time in BootTime.log for other platforms");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command:grep -i \"Wan_init_complete:\" /rdklogs/logs/BootTime.log)");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Wan init complete time is obtained.");
			LOGGER.info("**********************************************************************************");

			startTime = System.currentTimeMillis();
			String pattenMatcher = null;
			String searchLogMessage = null;
			do {
				searchLogMessage = BroadBandCommonUtils.concatStringUsingStringBuffer(
						BroadBandTraceConstants.ACTIVATION_WAN_INITIALIZATION_COMPLETE_NEW,
						AutomaticsConstants.DELIMITER_EQUALS);
				pattenMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(searchLogMessage,
						BroadBandTestConstants.PATTERN_FOR_UN_SIGNED_INT);
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.ACTIVATION_WAN_INITIALIZATION_COMPLETE_NEW,
						BroadBandCommandConstants.FILE_BOOT_TIME_LOG, BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

				if (CommonMethods.isNotNull(response)) {
					errorMessage = "Unable to get value of wan_init_complete from log message";
					response = CommonMethods.patternFinder(response, pattenMatcher);
					status = CommonMethods.isNotNull(response);
				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THIRTEEN_MINUTE_IN_MILLIS
					&& !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Wan init complete time is obtained - " + response);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 2 : OBTAIN BOOT_TO_WEBPA_READY_UPTIME VALUE FROM LOG MESSAGE IN
			 * BootTime.log
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;

			errorMessage = "Failed to find webpa ready uptime log message";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Obtain boot_to_WEBPA_READY_uptime value from log message in BootTime.log");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command:grep -i \"boot_to_WEBPA_READY_uptime\" /rdklogs/logs/BootTime.log");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPA ready uptime is obtained.");
			LOGGER.info("**********************************************************************************");

			try {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.WEBPA_READY_UPTIME, BroadBandCommandConstants.FILE_BOOT_TIME_LOG,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

				if (CommonMethods.isNotNull(response)) {
					try {
						startTime = Integer.parseInt(CommonMethods
								.patternFinder(response, BootTimePatterns.BOOT_TO_WEBPA_TIME.getPattern()).trim());
					} catch (Exception e) {
						errorMessage += "Failed to Parse the response to interger " + e.getMessage();
						LOGGER.error(errorMessage);
					}
					errorMessage = "Failed to get average value of Webpa ready time from stb props for model"
							+ device.getModel();

					String webpaReadyResponse = BroadBandCommonUtils.getWebpaReadyResponse(device);
					if (CommonMethods.isNotNull(webpaReadyResponse)) {
						try {
							expectTime = Integer.parseInt(webpaReadyResponse);
						} catch (Exception e) {
							errorMessage += "Failed to Parse the response to interger " + e.getMessage();
							LOGGER.error(errorMessage);
						}
						errorMessage = "WebPA ready time value: " + startTime + " is higher than model "
								+ device.getModel() + " average value: " + expectTime;
						status = startTime <= expectTime;
					}

				}
			} catch (Exception e) {
				errorMessage += e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : WebPA ready uptime obtained - " + startTime
						+ ", is lesser than the average value - " + expectTime);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 3 : VERIFY PARODUS PROCESS IS RUNNING
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to get pid for parodus process after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify parodus process is running");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command:pidof parodus");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Pid for parodus process is obtained.");
			LOGGER.info("**********************************************************************************");
			startTime = System.currentTimeMillis();
			do {
				status = CommonMethods.isNotNull(
						CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS));
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Pid for parodus process is obtained.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 4 : OBTAIN PARODUS STARTING TIME FROM TIMESTAMP IN LOG MESSAGE IN
			 * PARODUSLOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to get value of boot time from parameter and Certificate is not yet valid log message is present";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Obtain parodus starting time from timestamp in log message in PARODUSlog");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command:grep -i \"Starting component: Parodus\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Parodus starting time is obtained.");
			LOGGER.info("**********************************************************************************");
			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_BOOTTIME);
			if (CommonMethods.isNotNull(response)) {
				bootTime = Long.parseLong(response.trim());
				errorMessage = "Failed to find parodus starting log message";
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_STARTING_PAROUS, BroadBandCommandConstants.LOG_FILE_PARODUS,
						BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

				if (CommonMethods.isNotNull(response)) {
					errorMessage = "Unable to get parodus starting time from log message";
					startTime = BroadBandCommonUtils.getGMTEpochTimeFromUTCLogMessage(response) - bootTime;
					status = startTime > BroadBandTestConstants.CONSTANT_0;
					successMsg = "Parodus start time is obtained - " + startTime;
				}
			}
			if (!status) {
				status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_FOR_PARODUS_LOG, BroadBandCommandConstants.LOG_FILE_PARODUS,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
				successMsg = "Certificate is not yet valid log message did not appear as expected";

			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :" + successMsg);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 5 : VERIFY PARODUS IS CONNECTED TO SERVER OVER SSL LOG MESSAGE IN
			 * PARODUSLOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to find parodus connected to server log message";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify parodus is connected to server over SSL log message in PARODUSlog");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command:grep -i \"Connected to server over SSL\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Parodus is connected to server after reboot.");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Parodus is connected to server after reboot.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 6 : OBTAIN PARODUS CONNECTED TIME FROM LOG MESSAGE IN PARODUSLOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to find parodus connect_time-diff-boot_time log message";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Obtain parodus connected time from log message in PARODUSlog");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command:grep \"connect_time-diff-boot_time\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Parodus connected time is obtained.");
			LOGGER.info("**********************************************************************************");
			try {
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_CONNECT_TIME_DIFF_BOOT_TIME,
						BroadBandCommandConstants.LOG_FILE_PARODUS);
				if (CommonMethods.isNotNull(response)) {
					errorMessage = "Failed to obtain parodus connect time from log message";
					response = CommonMethods.patternFinder(response,
							BroadBandTestConstants.PATTERN_PARODUS_CONNECT_TIME);
					if (CommonMethods.isNotNull(response)) {
						try {
							startTime = Integer.parseInt(response.trim());
						} catch (Exception e) {
							errorMessage += "Failed to Parse the response to interger " + e.getMessage();
							LOGGER.error(errorMessage);
						}
						errorMessage = "Failed to get average value of Parodus uptime from stb props for model"
								+ device.getModel();

						String parodusResponse = BroadBandCommonUtils.getParodusConnectTime(device);
						if (CommonMethods.isNotNull(parodusResponse)) {
							try {
								expectTime = Integer.parseInt(parodusResponse);
							} catch (Exception e) {
								errorMessage += "Failed to Parse the response to interger " + e.getMessage();
								LOGGER.error(errorMessage);
							}
							errorMessage = "Parodus connect time value: " + startTime + " is higher than model "
									+ device.getModel() + " average value: " + expectTime;
							status = startTime <= expectTime;
						}
					}
				}
			} catch (Exception e) {
				errorMessage += e.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Parodus connected time obtained - " + startTime
						+ ", is lesser than the average value - " + expectTime);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 7 : OBTAIN WEBPA STARTING TIME FROM TIMESTAMP IN LOG MESSAGE IN WEBPALOG
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			errorMessage = "Failed to find webpa starting log message and webpa process is not up";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Obtain WebPA starting time from timestamp in log message in WEBPAlog");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command:grep -i \"Starting component: com.cisco.spvtg.ccsp.webpaagent\"/rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPA starting time is obtained.");
			LOGGER.info("**********************************************************************************");
			response = BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_STARTING_WEBPA, BroadBandCommandConstants.LOG_FILE_WEBPA);
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Unable to get webpa starting time from log message";
				startTime = BroadBandCommonUtils.getGMTEpochTimeFromUTCLogMessage(response) - bootTime;
				status = startTime > BroadBandTestConstants.CONSTANT_0;
				successMsg = "webpa start time is obtained - " + startTime;
			}
			if (!status) {
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
				successMsg = "Verified WebPA Processis up Successfully";
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL :" + successMsg);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * S8: Verify the Device manufacturer name using WebPA
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Get Parameter value failed to get device manufacturer name";
			String deviceManufacturerNameWebPA = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify the device manufacturer name using WebPA");
			LOGGER.info("STEP " + stepNumber + ": ACTION :Execute webpa command Device.DeviceInfo.Manufacturer");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Should be able to get the expected Device manufacturer name using WebPA Command");
			LOGGER.info("**********************************************************************************");

			String deviceManufacturerName = null;
			deviceManufacturerName = BroadBandCommonUtils.getDeviceManufacturerName(device);
			deviceManufacturerNameWebPA = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_DEVICE_MANUFACTURER_NAME);
			if (CommonMethods.isNotNull(deviceManufacturerNameWebPA)) {
				LOGGER.info("deviceManufacturerName is-" + deviceManufacturerNameWebPA);
				status = CommonMethods.isNotNull(deviceManufacturerNameWebPA)
						&& (deviceManufacturerNameWebPA.equalsIgnoreCase(deviceManufacturerName));
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Device Manufacturer name :" + deviceManufacturerNameWebPA
						+ ", is same as expected Manufacturer name : " + deviceManufacturerName);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * S9: Verify the Device manufacturer name in Parodus Log File
			 */
			stepNumber++;
			stepNum = "S" + stepNumber;
			status = false;
			errorMessage = "Device manufacturer names in Parodus Log file and Webpa are different";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify the device manufacturer name in Parodus log file");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute command: grep -i \"hw_manufacturer\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP " + stepNumber
					+ ":EXPECTED : Expected Device manufacturer name should be available on the parodus log file");
			LOGGER.info("**********************************************************************************");

			status = ParodusUtils.compareDeviceManufacturerName(tapEnv, device, deviceManufacturerNameWebPA);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Device manufacturer names in Parodus Log file and Webpa are same");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-1005");
	}

	/**
	 * Verify themis curl to acquire JWT for parodus
	 * <ol>
	 * <li>Verify create token script has been removed from /usr/ccsp/parodus
	 * directory</li>
	 * <li>Verify read token script has been removed from /usr/ccsp/parodus
	 * directory</li>
	 * <li>Verify encoded client certificate exists in /etc/parodus directory.</li>
	 * <li>Verify right serial number is printed by Parodus in Arm/Consolelog</li>
	 * <li>Verify right ECM mac is printed by Parodus in Arm/Consolelog</li>
	 * <li>Verify issuer curl response http_code 200 in PARODUSlog</li>
	 * <li>Verify curl success log message in PARODUSlog</li>
	 * <li>Verify decoded certificate exists in /tmp/adzvfchig-res.mch
	 * directory</li>
	 * <li>Verify client_cert_path is logged in PAROUDSlog</li>
	 * <li>Verify token_server_url is logged in PARODUSlog</li>
	 * <li>Verify decoded client cert present as command line argument in
	 * parodus</li>
	 * <li>Verify token server url present as command line argument in parodus</li>
	 * <li>Verify create token script has been removed as argument from parodus
	 * command line</li>
	 * <li>Verify read token script has been removed as argument from parodus
	 * command line</li>
	 * <li>Verify that parodus is connected to server by log message in
	 * PARODUSlog</li>
	 * <li>Clear PARODUS log and restart parodus process</li>
	 * <li>Verify issuer curl response http_code 200 in PARODUSlog after process
	 * restart</li>
	 * <li>Verify curl success log message in PARODUSlog after process restart</li>
	 * <li>Verify that parodus is connected to server by log message in PARODUSlog
	 * after process restart</li>
	 * </ol>
	 * 
	 * @author Ashwin sankara
	 * @refactor Govardhan
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PARODUS-1006")
	public void testVerifyParodusThemisCurl(Dut device) {

		// Variable Declaration begins
		// stores the test case id
		String testCaseId = "TC-RDKB-PARODUS-106";
		// stores the step number
		String stepNum = "s1";
		// stores the error message
		String errorMessage = null;
		// stores the command response
		String response = null;
		// stores the step result
		boolean status = false;
		// retrun curl retry failure message status
		boolean curlFailStatus = false;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1006");
		LOGGER.info("TEST DESCRIPTION: Verify themis curl to acquire JWT for parodus");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify create token script has been removed from /usr/ccsp/parodus directory");
		LOGGER.info("2. Verify read token script has been removed from /usr/ccsp/parodus directory");
		LOGGER.info("3. Verify encoded client certificate exists in /etc/parodus directory.");
		LOGGER.info("4. Verify right serial number is printed by Parodus in Arm/Consolelog");
		LOGGER.info("5. Verify right ECM mac is printed by Parodus in Arm/Consolelog");
		LOGGER.info("6. Verify issuer curl  response http_code 200 in PARODUSlog");
		LOGGER.info("7. Verify curl success log message in PARODUSlog");
		LOGGER.info("8. Verify decoded certificate exists in /tmp/adzvfchig-res.mch directory");
		LOGGER.info("9. Verify client_cert_path is logged in PAROUDSlog");
		LOGGER.info("10. Verify token_server_url is logged in PARODUSlog");
		LOGGER.info("11. Verify decoded client cert present as command line argument in parodus");
		LOGGER.info("12. Verify token server url present as command line argument in parodus");
		LOGGER.info("13. Verify create token script has been removed as argument from parodus command line");
		LOGGER.info("14. Verify read token script has been removed as argument from parodus command line");
		LOGGER.info("15. Verify that parodus is connected to server by log message in PARODUSlog");
		LOGGER.info("16. Clear PARODUS log and restart parodus process");
		LOGGER.info("17. Verify issuer curl  response http_code 200 in PARODUSlog after process restart");
		LOGGER.info("18. Verify curl success log message in PARODUSlog after process restart");
		LOGGER.info(
				"19. Verify that parodus is connected to server by log message in PARODUSlog after process restart");
		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			BroadBandPreConditionUtils.preConditionToRebootAndWaitForIpAccusition(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * Step 1 : Verify create token script has been removed from /usr/ccsp/parodus
			 * directory
			 */
			errorMessage = "Parodus create file script exists in /usr/ccsp/parodus directory";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Verify create token script has been removed from /usr/ccsp/parodus directory");
			LOGGER.info("STEP 1: ACTION : Execute command:ls /usr/ccsp/parodus/parodus_create_file.sh");
			LOGGER.info(
					"STEP 1: EXPECTED : File parodus_create_file.sh does not exist in /usr/ccsp/parodus directory.");
			LOGGER.info("**********************************************************************************");
			status = !CommonUtils.isFileExists(device, tapEnv,
					BroadBandCommandConstants.FILE_PATH_PARODUS_CREATE_FILE_SH);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL : File parodus_create_file.sh does not exist in /usr/ccsp/parodus directory.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 2 : Verify read token script has been removed from /usr/ccsp/parodus
			 * directory
			 */
			stepNum = "s2";
			errorMessage = "Parodus read file script exists in /usr/ccsp/parodus directory";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify read token script has been removed from /usr/ccsp/parodus directory");
			LOGGER.info("STEP 2: ACTION : Execute command:ls /usr/ccsp/parodus/parodus_read_file.sh");
			LOGGER.info("STEP 2: EXPECTED : File parodus_read_file.sh does not exist in /usr/ccsp/parodus directory.");
			LOGGER.info("**********************************************************************************");
			status = !CommonUtils.isFileExists(device, tapEnv,
					BroadBandCommandConstants.FILE_PATH_PARODUS_READ_FILE_SH);
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : File parodus_read_file.sh does not exist in /usr/ccsp/parodus directory.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 3 : Verify encoded client certificate exists in /etc/parodus directory.
			 */
			stepNum = "s3";
			errorMessage = "Unable to find file - /etc/ssl/certs/staticXpkiCrt.pk12";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify encoded client certificate exists in /etc/parodus directory.");
			LOGGER.info("STEP 3: ACTION : Execute command:ls /etc/ssl/certs/staticXpkiCrt.pk12");
			LOGGER.info("STEP 3: EXPECTED : File adzvfchig.mch exists in /etc/parodus directory and is encrypted");
			LOGGER.info("**********************************************************************************");
			if (CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_PATH_ENCRYPTED_PARODUS_CERT)) {
				errorMessage = "File /etc/parodus/adzvfchig.mch is not encrypted";
				status = BroadBandCommonUtils.isFileEncrypted(tapEnv, device,
						BroadBandCommandConstants.FILE_PATH_ENCRYPTED_PARODUS_CERT, false);
			}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : File adzvfchig.mch exists in /etc/parodus directory and is encrypted");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 4 : Verify right serial number is printed by Parodus in Arm/Consolelog
			 */
			stepNum = "s4";
			errorMessage = "Unable to find serial number log message in Arm/Consolelog";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify right serial number is printed by Parodus in Arm/Consolelog");
			LOGGER.info(
					"STEP 4: ACTION : Execute command:grep -i \"serialNumber returned from hal:\" /rdklogs/logs/Consolelog.txt.0 (ArmConsolelog.txt.0) for Atom Console Devices\nObtain value of Device.DeviceInfo.SerialNumber parameter and verify same value is logged.");
			LOGGER.info(
					"STEP 4: EXPECTED : Serial number printed by Parodus in Arm/Consolelog is same as WebPA parameter value");
			LOGGER.info("**********************************************************************************");
			response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.SYMBOL_QUOTES,
							BroadBandTraceConstants.LOG_MESSAGE_PARODUS_SER_NUM, BroadBandTestConstants.SYMBOL_QUOTES));
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Failed to obtain serial number from log message";
				response = CommonMethods.patternFinder(response,
						BroadBandTestConstants.PATTERN_MATCHER_PARODUS_SERIAL_NUMBER);
				if (CommonMethods.isNotNull(response)) {
					errorMessage = "Serial number printed by Parodus in Arm/Consolelog is different from serial number parameter value";
					status = BroadBandCommonUtils.compareSerialNumberWithParameterValue(tapEnv, device, response);
				}
			}
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Serial number printed by Parodus in Arm/Consolelog is same as WebPA parameter value");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 5 :Verify right ECM mac is printed by Parodus in Arm/Consolelog
			 */
			stepNum = "s5";
			errorMessage = "Unable to find device mac log message in Arm/Consolelog";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify right ECM mac is printed by Parodus in Arm/Consolelog");
			LOGGER.info(
					"STEP 5: ACTION : Execute command:\ngrep -i \"deviceMac\" /rdklogs/logs/Consolelog.txt.0 (ArmConsolelog.txt.0) for Atom Console Devices\nObtain value of Device.DeviceInfo.X_CISCO_COM_BaseMacAddress parameter and verify same value is logged.");
			LOGGER.info("STEP 5: EXPECTED : MAC printed by Parodus in Arm/Consolelog is same as WebPA parameter value");
			LOGGER.info("**********************************************************************************");
			response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.SYMBOL_QUOTES,
							BroadBandTraceConstants.LOG_MESSAGE_PARODUS_MAC, BroadBandTestConstants.SYMBOL_QUOTES));
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Failed to obtain MAC address from log message";
				response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_MATCHER_PARODUS_MAC);
				if (CommonMethods.isNotNull(response)) {
					errorMessage = "MAC printed by Parodus in Arm/Consolelog is different from CM MAC parameter value";
					status = BroadBandCommonUtils.compareCMMacWithParameterValue(tapEnv, device, response);
				}
			}
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : MAC printed by Parodus in Arm/Consolelog is same as WebPA parameter value");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 6 : Verify issuer curl response http_code 200 in PARODUSlog
			 */
			stepNum = "s6";
			errorMessage = "Unable to find response code 200 log message in PARODUSlog";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify issuer curl  response http_code 200 in PARODUSlog");
			LOGGER.info(
					"STEP 6: ACTION : Execute command:grep -i \"PARODUS: themis curl response 0 http_code 200\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 6: EXPECTED : Log message is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_HTTP_200, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Log message is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 7 : Verify curl success log message in PARODUSlog
			 */
			stepNum = "s7";
			errorMessage = "Unable to find curl success log message in PARODUSlog and getting "
					+ BroadBandTraceConstants.LOG_MESSAGE_TO_CHECK_PARODUS_CURL_RETRY_FAILURE + " Error Message";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify curl success log message in PARODUSlog");
			LOGGER.info(
					"STEP 7: ACTION : Execute command:grep \"Parodus: cURL success\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 7: EXPECTED : Log message is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CURL_SUCCESS,
					BroadBandCommandConstants.LOG_FILE_PARODUS));
			if (!status) {
				curlFailStatus = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_TO_CHECK_PARODUS_CURL_RETRY_FAILURE,
						BroadBandCommandConstants.LOG_FILE_PARODUS));
				if (curlFailStatus) {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CURL_SUCCESS,
							BroadBandCommandConstants.LOG_FILE_PARODUS));
				}
			}
			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Curl success log message is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 8 : Verify decoded certificate exists in /tmp/adzvfchig-res.mch
			 * directory
			 */
			stepNum = "s8";
			errorMessage = "Unable to find file - /tmp/adzvfchig-res.mch";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify decoded certificate exists in /tmp/adzvfchig-res.mch directory");
			LOGGER.info("STEP 8: ACTION : Execute command:ls /tmp/adzvfchig-res.mch");
			LOGGER.info("STEP 8: EXPECTED : File adzvfchig-res.mch exists in /tmp directory");
			LOGGER.info("**********************************************************************************");
			status = CommonUtils.isFileExists(device, tapEnv,
					BroadBandCommandConstants.FILE_PATH_DECRYPTED_PARODUS_CERT);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : File adzvfchig-res.mch exists in /tmp directory");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 9 : Verify client_cert_path is logged in PAROUDSlog
			 */
			stepNum = "s9";
			errorMessage = "Unable to find client_cert_path log message in PARODUSlog";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify client_cert_path is logged in PAROUDSlog");
			LOGGER.info("STEP 9: ACTION : Execute command:grep client_cert_path /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 9: EXPECTED : Client cert path is logged as /tmp/adzvfchig-res.mch");
			LOGGER.info("**********************************************************************************");
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CLIENT_CERT_PATH, BroadBandCommandConstants.LOG_FILE_PARODUS);
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Client cert path is not expected value - "
						+ BroadBandCommandConstants.FILE_PATH_DECRYPTED_PARODUS_CERT;
				status = CommonMethods.patternMatcher(response,
						BroadBandCommandConstants.FILE_PATH_DECRYPTED_PARODUS_CERT);
			}
			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Client cert path is logged as /tmp/adzvfchig-res.mch");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			// Loading the PARODUS_TOKEN_SERVER_URL from props
			String PARODUS_TOKEN_SERVER_URL = BroadbandPropertyFileHandler.getParodusTokenServerURL();
			/**
			 * Step 10 : Verify token_server_url is logged in PARODUSlog"
			 */
			stepNum = "s10";
			errorMessage = "Unable to find token_server_url log message in PARODUSlog";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify token_server_url is logged in PARODUSlog");
			LOGGER.info("STEP 10: ACTION : Execute command:grep token_server_url /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 10: EXPECTED : Token server url is logged as " + PARODUS_TOKEN_SERVER_URL);
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_TOKEN_SERVER_URL, BroadBandCommandConstants.LOG_FILE_PARODUS);
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Token server url is not expected value - " + PARODUS_TOKEN_SERVER_URL;
				status = CommonMethods.patternMatcher(response, PARODUS_TOKEN_SERVER_URL);
			}
			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Token server url is logged as " + PARODUS_TOKEN_SERVER_URL);
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 11 : Verify decoded client cert present as command line argument in
			 * parodus
			 */
			stepNum = "s11";
			errorMessage = "Unable to find decoded cert in /tmp/parodusCmd.cmd";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION : Verify decoded client cert present as command line argument in parodus");
			LOGGER.info("STEP 11: ACTION : Execute command:grep -i \"/tmp/adzvfchig-res.mch\" /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 11: EXPECTED : File contains decoded client cert as argument");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandCommandConstants.FILE_PATH_DECRYPTED_PARODUS_CERT,
					BroadBandCommandConstants.FILE_PATH_PARODUS_CMD));
			if (status) {
				LOGGER.info("STEP 11: ACTUAL : File contains decoded client cert as argument");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 12 : Verify token server url present as command line argument in parodus
			 */
			stepNum = "s12";
			errorMessage = "Unable to find token server url in /tmp/parodusCmd.cmd";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Verify token server url present as command line argument in parodus");
			LOGGER.info("STEP 12: ACTION : Execute command:grep -i \" +" + PARODUS_TOKEN_SERVER_URL
					+ "+\" /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 12: EXPECTED : File contains token server url as argument");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					PARODUS_TOKEN_SERVER_URL, BroadBandCommandConstants.FILE_PATH_PARODUS_CMD));
			if (status) {
				LOGGER.info("STEP 12: ACTUAL : File contains token server url as argument");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 13 : Verify create token script has been removed as argument from
			 * parodus command line
			 */
			stepNum = "s13";
			errorMessage = "Able to find create token script in /tmp/parodusCmd.cmd";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 13: DESCRIPTION : Verify create token script has been removed as argument from parodus command line");
			LOGGER.info(
					"STEP 13: ACTION : Execute command:grep -i \"/usr/ccsp/parodus/parodus_create_file.sh\" /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 13: EXPECTED : File does not contain create token script as argument");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CMD_CREATE_FILE_ARGUEMENT,
					BroadBandCommandConstants.FILE_PATH_PARODUS_CMD));
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : File does not contain create token script as argument");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 14 : Verify read token script has been removed as argument from parodus
			 * command line
			 */
			stepNum = "s14";
			errorMessage = "Able to find read token script in /tmp/parodusCmd.cmd";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 14: DESCRIPTION : Verify read token script has been removed as argument from parodus command line");
			LOGGER.info(
					"STEP 14: ACTION : Execute command:grep -i \"/usr/ccsp/parodus/parodus_read_file.sh\" /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 14: EXPECTED : File does not contain read token script as argument");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CMD_READ_FILE_ARGUEMENT,
					BroadBandCommandConstants.FILE_PATH_PARODUS_CMD));
			if (status) {
				LOGGER.info("STEP 14: ACTUAL : File does not contain read token script as argument");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 15 : Verify that parodus is connected to server by log message in
			 * PARODUSlog
			 */
			stepNum = "s15";
			errorMessage = "Unable to find parodus connected log message in PARODUSlog";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 15: DESCRIPTION : Verify that parodus is connected to server by log message in PARODUSlog");
			LOGGER.info(
					"STEP 15: ACTION : Execute command:grep -i \"Connected to server over SSL\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 15: EXPECTED : Connected to server log message is present in PARODUSlog file");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 15: ACTUAL : Connected to server log message is present in PARODUSlog file");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 16 : Clear PARODUS log and restart parodus process
			 */
			stepNum = "s16";
			errorMessage = "Failed to restart parodus process";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION : Clear PARODUS log and restart parodus process");
			LOGGER.info(
					"STEP 16: ACTION : Execute commands:\n1. echo > /rdklogs/logs/PARODUSlog.txt.0\n2. pidof parodus\n3. kill -11 <pid>\n4. pidof parodus");
			LOGGER.info("STEP 16: EXPECTED : Parodus process restarted successfully");
			LOGGER.info("**********************************************************************************");
			CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_PARODUS);
			status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
					BroadBandTestConstants.PROCESS_NAME_PARODUS);
			if (status) {
				LOGGER.info("STEP 16: ACTUAL : Parodus process restarted successfully");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * Step 17 : Verify issuer curl response http_code 200 in PARODUSlog after
			 * process restart
			 */
			stepNum = "s17";
			errorMessage = "Unable to find response code 200 log message in PARODUSlog after process restart";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 17: DESCRIPTION : Verify issuer curl  response http_code 200 in PARODUSlog after process restart");
			LOGGER.info(
					"STEP 17: ACTION : Execute command:grep -i \"PARODUS: themis curl response 0 http_code 200\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 17: EXPECTED : Log message is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_HTTP_200, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 17: ACTUAL : Log message is present in Arm/Consolelog");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 18 : Verify curl success log message in PARODUSlog
			 */
			stepNum = "s18";
			errorMessage = "Unable to find curl success log message in PARODUSlog and getting "
					+ BroadBandTraceConstants.LOG_MESSAGE_TO_CHECK_PARODUS_CURL_RETRY_FAILURE + " Error Message";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 18: DESCRIPTION : Verify curl success log message in PARODUSlog");
			LOGGER.info(
					"STEP 18: ACTION : Execute command:grep \"Parodus: cURL success\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 18: EXPECTED : Curl success log message is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CURL_SUCCESS,
					BroadBandCommandConstants.LOG_FILE_PARODUS));
			if (!status) {
				curlFailStatus = CommonMethods.isNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_TO_CHECK_PARODUS_CURL_RETRY_FAILURE,
						BroadBandCommandConstants.LOG_FILE_PARODUS));
				if (curlFailStatus) {
					status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
							BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CURL_SUCCESS,
							BroadBandCommandConstants.LOG_FILE_PARODUS));
				}
			}
			if (status) {
				LOGGER.info("STEP 18: ACTUAL : Curl success log message is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 19 : Verify that parodus is connected to server by log message in
			 * PARODUSlog after process restart
			 */
			stepNum = "s19";
			errorMessage = "Unable to find parodus connected log message in PARODUSlog after process restart";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 19: DESCRIPTION : Verify that parodus is connected to server by log message in PARODUSlog after process restart");
			LOGGER.info(
					"STEP 19: ACTION : Execute command:grep -i \"Connected to server over SSL\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 19: EXPECTED : Connected to server log message is present in PARODUSlog file");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 19: ACTUAL : Connected to server log message is present in PARODUSlog file");
			} else {
				LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-1006");
	}

	/**
	 * Verify Integrate parodus2ccsp with Yocto
	 * <ol>
	 * <li>Verify WebPa process is connected to parodus</li>
	 * <li>Verify WebPa GET request is working</li>
	 * <li>Verify WebPa GET request for requested parameter logged in
	 * WEBPAlog.txt.0</li>
	 * <li>verify webPa Get-Attributes request is working</li>
	 * <li>Verify Get-Attributes request is logged in WEBPAlog.txt.0</li>
	 * <li>verify webPa Set-Attributes request is working</li>
	 * <li>Verify Set-Attributes request is logged in WEBPAlog.txt.0</li>
	 * <li>verify TEST and SET requests are working</li>
	 * <li>Verify TEST and SET request logged in WEBPAlog.txt.0</li>
	 * <li>verify POST request is working</li>
	 * <li>Verify POST request logged in WEBPAlog.txt.0</li>
	 * <li>verify PUT request is working</li>
	 * <li>Verify PUT request is logged in WEBPAlog.txt.0</li>
	 * <li>verify DELETE is working</li>
	 * <li>Verify DELETE request is logged in WEBPAlog.txt.0</li>
	 * <li>Get 5GHz private SSID using WEBPA get operation</li>
	 * <li>Update 5GHz private SSID name using webpa</li>
	 * <li>Verify notification for stack is logged in WEBPAlog.txt.0</li>
	 * </ol>
	 * 
	 * @refactor Govardhan
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.SYSTEM })
	@TestDetails(testUID = "TC-RDKB-PARODUS-1007")
	public void testVerifyParodusToCcsp(Dut device) throws IOException, JSONException {

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1007");
		LOGGER.info("TEST DESCRIPTION: Verify Integrate parodus2ccsp with Yocto");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify WebPa process is connected to parodus ");
		LOGGER.info("2. Verify WebPa GET request   is working");
		LOGGER.info("3. Verify WebPa GET request for requested parameter logged in WEBPAlog.txt.0");
		LOGGER.info("4. verify webpa Get-Attributes request is working ");
		LOGGER.info("5. Verify Get-Attributes  request is logged in WEBPAlog.txt.0 ");
		LOGGER.info("6. verify webpa Set-Attributes request is working ");
		LOGGER.info("7. Verify Set-Attributes request is logged in WEBPAlog.txt.0");
		LOGGER.info("8. verify TEST and SET requests are working ");
		LOGGER.info("9. Verify TEST and SET request logged in WEBPAlog.txt.0");
		LOGGER.info("10. verify POST request is working ");
		LOGGER.info("11. Verify POST request logged in WEBPAlog.txt.0");
		LOGGER.info("12. verify PUT request is working ");
		LOGGER.info("13. Verify PUT request is logged in WEBPAlog.txt.0");
		LOGGER.info("14. verify DELETE is working ");
		LOGGER.info("15. Verify DELETE request is logged in WEBPAlog.txt.0");
		LOGGER.info("16. Get 5GHz private SSID using WEBPA get operation");
		LOGGER.info("17. Update 5GHz private SSID name using webpa");
		LOGGER.info("18. Verify notification for stack is logged in WEBPAlog.txt.0");

		LOGGER.info("#######################################################################################");

		// variable to store errorMessage
		String errorMessage = null;
		// variable to store testcaseID
		String testCaseId = "TC-RDKB-PARODUS-007";
		// variable to store teststepNumber
		String stepNumber = "s1";
		// variable to store status
		boolean status = false;
		// Variable to store response
		String response = null;
		// string to store the webpa server response
		WebPaServerResponse webPaServerResponse = null;
		boolean isAtomSyncDevice = false;
		String tableRowNumber = null;
		String privateSsid = null;
		boolean ssidStatus = false;

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 1: DESCRIPTION : Reboot the device");
			LOGGER.info("PRE-CONDITION 1: ACTION : Execute reboot command on the console");
			LOGGER.info("PRE-CONDITION 1: EXPECTED : Device is rebooted successfully");
			LOGGER.info("#######################################################################################");
			isAtomSyncDevice = CommonMethods.isAtomSyncAvailable(device, tapEnv);
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Device is rebooted successfully");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : Device is not rebooted");
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}

			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify WebPa process is connected to parodus ");
			LOGGER.info(
					"STEP 1: ACTION : Execute command: grep -i \"Init for parodus Success\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 1: EXPECTED : Response should contain the log message as Init for parodus Success");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message from /rdklogs/logs/WEBPAlog.txt.0";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_INIT_PARODUS,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Successfully verified Init for parodus success");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s2";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : verify webpa Get-Attributes request is working ");
			LOGGER.info(
					"STEP 2: ACTION : Execute WebPa command for Device.DeviceInfo.X_COMCAST-COM_CM_IP,Device.X_Comcast_com_ParentalControl.RollbackUTC_Local&attributes=no");
			LOGGER.info("STEP 2: EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			// waiting for 5 minutes to get the logs
			tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
			status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL, BroadBandTestConstants.NOTIFY);
			errorMessage = "Failed to get the webpa request for parameter:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL;
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL : Successfully verified get attributes request through webpa command for parameter :"
								+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL);
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s3";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify Get-Attributes  request is logged in WEBPAlog.txt.0 ");
			LOGGER.info("STEP 3: ACTION : Execute command: grep \"GET_ATTRIBUTES\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 3: EXPECTED : Response should contain the log message for Get-attributes");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for GET_ATTRIBUTES request";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_GET_ATTRIBUTES,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfully verified the get attributes in log message");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify WebPa GET request   is working");
			LOGGER.info("STEP 4: ACTION : Execute WebPa command for Device.X_RDKCENTRAL-COM_Webpa.Version");
			LOGGER.info("STEP 4: EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.X_RDKCENTRAL-COM_Webpa.Version";
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WEBPA_VERSION);
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Successfully verified the get request using webpa");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s5";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info(
					"STEP 5: DESCRIPTION : Verify WebPa GET request for requested parameter logged in WEBPAlog.txt.0");
			LOGGER.info("STEP 5: ACTION : Execute command: grep \"GET\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info(
					"STEP 5: EXPECTED : Response should contain the log message for GET request of WebPa parameter");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for GET request";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_GET, isAtomSyncDevice);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Successfully verified the log message for webpa GET in log message");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : verify webpa Set-Attributes request is working ");
			LOGGER.info(
					"STEP 6: ACTION : Execute webpa set operation for Device.NotifyComponent.X_RDKCENTRAL-COM_Connected-Client,attributes:{ notify: 1 }");
			LOGGER.info("STEP 6: EXPECTED : The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");

			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.STRING_TEST_1, BroadBandTestConstants.CONSTANT_0);
			errorMessage = CommonUtils.concatStringUsingStringBuffer(
					"Failed to Set Attribute value for WebPa parameter:",
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully Set Attribute for webpa parameter:"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			if (!DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("***********************************************************");
				LOGGER.info("STEP 7: DESCRIPTION : Verify Set-Attributes request is logged in WEBPAlog.txt.0");
				LOGGER.info("STEP 7: ACTION : Execute command:grep \"SET ATTRIBUTES\" /rdklogs/logs/WEBPAlog.txt.0 ");
				LOGGER.info("STEP 7: EXPECTED : Response should contain the log message for Set-Attributes request");
				LOGGER.info("***********************************************************");
				errorMessage = "Failed to get the log message for SET ATTRIBUTES request";
				status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_SET_ATTRIBUTES,
						isAtomSyncDevice);
				if (status) {
					LOGGER.info(
							"STEP 7: ACTUAL: Successfully verified the log message for webpa set attributes in log message");
				} else {
					LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
				}
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}
			// ##################################################################################################//

			stepNumber = "s8";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : verify TEST and SET requests are working ");
			LOGGER.info(
					"STEP 8: ACTION : {parameters: [{name: Device.WiFi.Radio.10100.Enable,value: true,dataType: 3,attributes: {notify: 1}},{name: Device.WiFi.Radio.10000.Enable,value: true,dataType: 3,attributes: {notify: 1}");
			LOGGER.info("STEP 8: EXPECTED : The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to Set and Test value for WebPa parameter:";

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Successfully Verified Test and set request using webpa");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s9";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify TEST and SET request logged in WEBPAlog.txt.0");
			LOGGER.info("STEP 9: ACTION : Execute command:grep \"Test and Set Request\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 9: EXPECTED : Response should contain Test and Set request log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for TEST and SET request";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_TEST_SET, isAtomSyncDevice);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL: Successfully verified the log message for webpa TEST and SET in log message");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s10";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : verify POST request is working ");
			LOGGER.info(
					"STEP 10: ACTION : webpa POST operation for Device.X_Comcast_com_ParentalControl.ManagedDevices.Device.");
			LOGGER.info(
					"STEP 10: EXPECTED : The webpa command should Execute successfully or failed with server response");
			LOGGER.info("***********************************************************");
			try {
				errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter by Webpa POST command";
				Map<String, List<String>> macFilterTable = new HashMap<String, List<String>>();
				List<String> MacAddressList = new ArrayList<String>();
				MacAddressList.add(BroadBandTestConstants.DUMMY_MAC);
				List<String> Type = new ArrayList<String>();
				Type.add(BroadBandTestConstants.BLOCK);
				List<String> Description = new ArrayList<String>();
				Description.add(BroadBandTestConstants.STRING_TEST_1);
				// adding to the Map.
				macFilterTable.put(BroadBandTestConstants.MACADDRESS, MacAddressList);
				macFilterTable.put(BroadBandTestConstants.TYPE, Type);
				macFilterTable.put(BroadBandTestConstants.DESCRIPTION, Description);
				webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
						BroadBandWebPaConstants.WEBPA_PARAM_PARENTAL_CONTROL_MANAGED_DEVICES, macFilterTable);
				// Saving the Table Row number to do delete operation in further
				// steps.
				tableRowNumber = webPaServerResponse.getRow();
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
				if (status) {
					LOGGER.info(
							"STEP 10: ACTUAL: Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command");
				} else {
					LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
				}
			} catch (Exception message) {
				errorMessage = "failed to get the response from server side";
				status = true;
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s11";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify POST request logged in WEBPAlog.txt.0");
			LOGGER.info("STEP 11: ACTION : Execute command:grep \"ADD_ROW Request\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 11: EXPECTED : Response should contain POST request log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for POST request";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_ADD_ROW_REQ,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL: Successfully verified the log message for webpa ADD_ROW request in log message");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s12";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : verify PUT request is working ");
			LOGGER.info("STEP 12: ACTION : Execute Webpa PUT request for Device.DHCPv4.Server.Pool.1.StaticAddress. ");
			LOGGER.info(
					"STEP 12: EXPECTED : The webpa command should Execute successfully or failed with server response");
			LOGGER.info("***********************************************************");
			try {
				errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter by Webpa POST command";
				HashMap<String, HashMap<String, List<String>>> map = new HashMap<String, HashMap<String, List<String>>>();
				Map<String, List<String>> macFilterTable1 = new HashMap<String, List<String>>();
				List<String> Yiaddr = new ArrayList<>();
				Yiaddr.add(BroadBandConnectedClientUtils.getReservedIpBetweenDhcpRangeFromRouter(tapEnv, device,
						BroadBandTestConstants.STRING_NULL_IP));
				macFilterTable1.put(BroadBandTestConstants.IPADDRESS, Yiaddr);
				map.put(BroadBandTestConstants.STRING_ZERO, (HashMap<String, List<String>>) macFilterTable1);
				LOGGER.info("Printing Map Value:" + map);
				webPaServerResponse = tapEnv.putWebpaTableParamUsingRestApi(device,
						BroadBandWebPaConstants.WEBPA_PARAM_SERVER_POOL_STATIC_ADDRESS, map);
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
				if (status) {
					LOGGER.info(
							"STEP 12: ACTUAL: Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command");
				} else {
					LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
				}
			} catch (Exception message) {
				errorMessage = "Faile to get webpa response from server for PUT request";
				status = true;
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s13";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Verify PUT request is logged in WEBPAlog.txt.0");
			LOGGER.info("STEP 13: ACTION : Execute command:grep \"REPLACE_ROWS\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 13: EXPECTED : Response should contain PUT request log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for PUT request";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_REPLACE_ROWS,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info(
						"STEP 13: ACTUAL: Successfully verified the log message for webpa REPLACE_ROWS in log message");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s14";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : verify DELETE is working ");
			LOGGER.info(
					"STEP 14: ACTION :Webpa DELETE request for Device.X_Comcast_com_ParentalControl.ManagedDevices.Device");
			LOGGER.info("STEP 14: EXPECTED : The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			errorMessage = "Unable to delete row using rest Api for the given table row number";
			if (CommonMethods.isNotNull(tableRowNumber)) {
				webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
				LOGGER.info(webPaServerResponse.toString());
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			if (status) {
				LOGGER.info("STEP 14: ACTUAL: Successfully deleted the row from the table");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s15";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 15: DESCRIPTION : Verify DELETE request is logged in WEBPAlog.txt.0");
			LOGGER.info("STEP 15: ACTION : Execute command:grep DELETE_ROW /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 15: EXPECTED : Response should contain DELETE request log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for DELETE request";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_DELETE_ROWS,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info(
						"STEP 15: ACTUAL: Successfully verified the log message for webpa DELETE_ROW in log message");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s16";
			status = false;
			LOGGER.info("****************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION: Get 5GHz private SSID using WEBPA get operation");
			LOGGER.info("STEP 16: ACTION: Webpa get command: Device.WiFi.SSID.10101.SSID");
			LOGGER.info("STEP 16: EXPECTED: Webpa should execute successfully");
			LOGGER.info("****************************************************************");
			errorMessage = "Failed to get response for Webpa paramter Device.WiFi.SSID.10101.SSID";
			privateSsid = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID);
			status = CommonMethods.isNotNull(privateSsid);
			if (status) {
				LOGGER.info("STEP 16: ACTUAL: Successfully collected the 5GHz private SSID using webpa");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s17";
			LOGGER.info("****************************************************************");
			LOGGER.info("STEP 17: DESCRIPTION: Update 5GHz private SSID name using webpa");
			LOGGER.info("STEP 17: ACTION: Webpa set command: Device.WiFi.SSID.10101.SSID");
			LOGGER.info("STEP 17: EXPECTED: 5GHz private SSID should update with webpa");
			LOGGER.info("****************************************************************");
			errorMessage = "Failed to update the 5GHz private SSID for Webpa parameter Device.WiFi.SSID.10101.SSID";
			ssidStatus = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.TEST_SSID_5,
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 17: ACTUAL: Successfully updated 5GHz private SSID to previous value using webpa");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, ssidStatus, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s18";
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 18: DESCRIPTION : Verify notification for stack is logged in WEBPAlog.txt.0");
			LOGGER.info(
					"STEP 18: ACTION : Execute command:grep -i \"Notification Event from stack\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info(
					"STEP 18: EXPECTED : Response should contain Notification Event from stack request log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for Notification event from stack";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_NOTIFY_STACK,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info(
						"STEP 18: ACTUAL: Successfully verified the log message for Notification from stack in log message");
			} else {
				LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying Integrate parodus2ccsp with Yocto" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			if (ssidStatus) {
				LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
				LOGGER.info("POST-CONDITION STEPS");
				LOGGER.info("POST-CONDITION: DESCRIPTION: Revert back to previous 5GHz private SSID name using webpa");
				LOGGER.info("POST-CONDITION: ACTION: Webpa set command: Device.WiFi.SSID.10101.SSID");
				LOGGER.info(
						"POST-CONDITION: EXPECTED: 5GHz private SSID should update with previous value using webpa ");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID,
						BroadBandTestConstants.CONSTANT_0, privateSsid,
						BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				LOGGER.info("POST-CONDITION: ACTUAL: Successfully reverted back 5GHz private SSID");
				LOGGER.info("POST-CONFIGURATIONS: FINAL STATUS - " + status);
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}

		}
	}

	/**
	 * Verify Integrate parodus2ccsp with Yocto
	 * <ol>
	 * <li>1. Verify system ready check in WEBPAlog.txt.0</li>
	 * <li>2. Verify component caching is completed in WEBPAlog.txt.0</li>
	 * <li>3. Verify telemetry logging for reboot reason in WEBPAlog.txt.0</li>
	 * <li>4. Verify telemetry logging for Failed to get component object in
	 * WEBPAlog.txt.0</li>
	 * <li>5. Verify telemetry logging for webpa ready in WEBPAlog.txt.0</li>
	 * <li>6. Verify webpa agent log message in WEBPAlog.txt.0</li>
	 * <li>7. Verify WebPa process is running in device</li>
	 * <li>8. Clear the core log file in device</li>
	 * <li>9. Kill and verify the webpa process</li>
	 * <li>10. Verify minidumps file uploaded in core log file</li>
	 * <li>11. Verify PARODUS_ENABLE file is not present in ARM console</li>
	 * <li>12. Verify PARODUS_ENABLE file is not present in ATOM console</li>
	 * </ol>
	 * 
	 * @refactor Govardhan
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.SYSTEM })
	@TestDetails(testUID = "TC-RDKB-PARODUS-1008")
	public void testVerifyParodusToCcspProcess(Dut device) throws IOException, JSONException {

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1008");
		LOGGER.info("TEST DESCRIPTION: Verify Integrate parodus2ccsp with Yocto");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify telemetry logging for webpa ready in Boottime.log");
		LOGGER.info("2. Verify system ready check in WEBPAlog.txt.0");
		LOGGER.info("3. Verify component caching is completed in WEBPAlog.txt.0");
		LOGGER.info("4. Verify telemetry logging for reboot reason in WEBPAlog.txt.0");
		LOGGER.info("5. Verify telemetry logging for Failed to get component object in WEBPAlog.txt.0");
		LOGGER.info("6. Verify webpa agent log message in WEBPAlog.txt.0");
		LOGGER.info("7. Verify WebPa process is running in device");
		LOGGER.info("8. Clear the core log file in device");
		LOGGER.info("9. Kill and verify the webpa process");
		LOGGER.info("10. Verify minidumps file uploaded in core log file");
		LOGGER.info("11. Verify PARODUS_ENABLE file is not present in ARM console");
		LOGGER.info("12. Verify PARODUS_ENABLE file is not present in ATOM console");
		LOGGER.info("#######################################################################################");

		// variable to store errorMessage
		String errorMessage = null;
		// variable to store testcaseID
		String testCaseId = "TC-RDKB-PARODUS-008";
		// variable to store teststepNumber
		int stepNum = 1;
		String stepNumber = "s" + stepNum;
		// variable to store status
		boolean status = false;
		// Variable to store response
		String response = null;
		// variable to store command
		String command = null;
		boolean isAtomSyncDevice = false;
		boolean isSystemReady = false;
		long startTime = BroadBandTestConstants.CONSTANT_0;

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION 1: DESCRIPTION : Reboot the device");
			LOGGER.info("PRE-CONDITION 1: ACTION : Execute reboot command on the console");
			LOGGER.info("PRE-CONDITION 1: EXPECTED : Device is rebooted successfully");
			LOGGER.info("#######################################################################################");
			isAtomSyncDevice = CommonMethods.isAtomSyncAvailable(device, tapEnv);
			status = BroadBandCommonUtils.rebootAndWaitForStbAccessible(device, tapEnv);
			if (DeviceModeHandler.isRPIDevice(device)) {
				LOGGER.info("clearing the minidumps folder if RPi device :");
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TO_REMOVE,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandTestConstants.STRING_PARTITION_MINIDUMPS, "/*dmp"));
			}
			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Device is rebooted successfully");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : Device is not rebooted");
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}

			stepNumber = "S" + stepNum;
			status = false;

			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify telemetry logging for webpa ready in BootTime.log");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute command:grep -I \"WEBPA_READY\" /rdklogs/logs/BootTime.log");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message WEBPA_READY in BootTime.log";
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_WEBPA_READY, BroadBandCommandConstants.FILE_BOOT_TIME_LOG,
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Successfully verified the log message WEBPA_READY in BootTime.log file");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify system ready check in WEBPAlog.txt.0");
			LOGGER.info(
					"STEP " + stepNum + ": ACTION : Execute command:grep -I \"Checked\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain log message of system ready check");
			LOGGER.info("***********************************************************");

			errorMessage = "Failed to get the log message checked in /rdklogs/logs/WEBPAlog.txt.0";
			status = BroadBandSystemUtils.searchSystemReadySignalLogInArmOrAtom(device, tapEnv, isAtomSyncDevice);
			isSystemReady = status;
			if (status) {
				LOGGER.info(
						"STEP " + stepNum + ": ACTUAL: Successfully verified the system ready check in WEBPA log file");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify component caching is completed in WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION :Execute command:grep -i \"Component caching is completed\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the component caching log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message component caching is completed in WEBPAlog.txt.0";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_COMP_CACHING,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Successfully verified the component caching completed in WEBPA log file");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info(
					"STEP " + stepNum + ": DESCRIPTION : Verify telemetry logging for reboot reason in WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute command:grep -i \"Received reboot_reason as:\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the log message of reboot reason");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message received reboot reason in WEBPAlog.txt.0";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_REBOOT_REASON,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Successfully verified the log message received reboot_reeason in WEBPA log file");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum
					+ ": DESCRIPTION : Verify telemetry logging for Failed to get component object in WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute command:grep -I \"Failed to\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the log message");
			LOGGER.info("***********************************************************");
			if (!isSystemReady) {
				errorMessage = "Failed to get the log message failed to in WEBPAlog.txt.0";
				status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_FAILED_TO,
						isAtomSyncDevice);
				if (status) {
					LOGGER.info("STEP " + stepNum
							+ ": ACTUAL: Successfully verified the log message Failed to in WEBPA log file");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				LOGGER.info("STEP " + stepNum
						+ " : ACTUAL : system is ready and component caching is successful so failed message checking is skipped");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.NA_MSG_FOR_SYSTEM_READY_SIGNAL, false);
			}

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify webpa agent log message in WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute command:grep -I \"webpaagent\" /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the log message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message webpaagent in WEBPAlog.txt.0";
			status = searchWebpaLogInArmOrAtom(device, BroadBandTraceConstants.LOG_MESSAGE_WEBPA_AGENT,
					isAtomSyncDevice);
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Successfully verified the log message webpaagent in WEBPA log file");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify WebPa process is running in device");
			LOGGER.info("STEP " + stepNum + ": ACTION : Execute command:pidof webpa");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the process id of webpa");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the process id for webpa";
			startTime = System.currentTimeMillis();
			do {
				response = (isAtomSyncDevice
						? BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
								BroadBandTestConstants.PROCESS_NAME_WEBPA)
						: CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_WEBPA));
				status = CommonMethods.isNotNull(response);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS
					&& !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP " + stepNum
						+ ": ACTUAL: Successfully verified webpa process running in device & PID of webpa is: "
						+ response);
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("****************************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION: Clear the core log file in device");
			LOGGER.info("STEP " + stepNum + ": ACTION: echo \" \" > /rdklogs/logs/core_log.txt");
			LOGGER.info("STEP " + stepNum + ": EXPECTED: Log file should be cleared ");
			LOGGER.info("****************************************************************");
			errorMessage = "Failed to clear the core log file";
			if (isAtomSyncDevice) {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_ECHO_WITH_SPACE,
						BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB);
				CommonMethods.executeCommandInAtomConsole(device, tapEnv, command);
				response = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
								BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB));
				status = CommonMethods.isNull(response);
			} else {
				status = CommonUtils.clearLogFile(tapEnv, device, BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB);
			}
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL: Successfully cleared the core log file");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Kill and verify the webpa process");
			LOGGER.info("STEP " + stepNum + ": ACTION : Execute command:kill -11 <pid>");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : webpa process should kill");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to kill the process webpa";
			status = (isAtomSyncDevice
					? BroadBandCommonUtils.killProcessAndVerifyInAtomConsole(tapEnv, device,
							BroadBandTestConstants.PROCESS_NAME_WEBPA)
					: CommonMethods.restartProcess(device, tapEnv, ProcessRestartOption.KILL_11,
							BroadBandTestConstants.PROCESS_NAME_WEBPA));
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL: Successfully killed the process webpa");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;

			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify minidumps file uploaded in core log file");
			LOGGER.info("STEP " + stepNum
					+ ": ACTION : Execute command:grep -i \"minidump Upload is successful\" /rdklogs/logs/core_log.txt");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the minidump upload success message");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to get the log message for minidump upload successful in core_log.txt";
			if (!DeviceModeHandler.isRPIDevice(device)) {
				response = (isAtomSyncDevice ? BroadBandCommonUtils.searchLogFilesInAtomConsoleByPolling(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_UPLOAD_SUCCESS,
						BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB,
						BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
						: BroadBandCommonUtils.searchLogFiles(tapEnv, device,
								BroadBandTraceConstants.LOG_MESSAGE_UPLOAD_SUCCESS,
								BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB,
								BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
				status = CommonMethods.isNotNull(response);
			} else {
				LOGGER.info("waiting 3 minutes...");
				tapEnv.waitTill(BroadBandTestConstants.THIRTEEN_MINUTE_IN_MILLIS);
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_LS, BroadBandTestConstants.STRING_PARTITION_MINIDUMPS));
				LOGGER.info("contents of minidump folder :" + response);
				status = CommonMethods.isNotNull(response);
			}
			if (status) {
				LOGGER.info(
						"STEP " + stepNum + ": ACTUAL: Successfully verified crash file uploaded for webpa process");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			errorMessage = "PARODUS_ENABLE File is present in device under ARM console";
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP " + stepNum + ": DESCRIPTION : Verify PARODUS_ENABLE file does not exist in ARM console");
			LOGGER.info("STEP " + stepNum + ": ACTION : Execute command:find / -iname PARODUS_ENABLE");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should contain the PARODUS_ENABLE");
			LOGGER.info("***********************************************************");

			status = !BroadBandCommonUtils.isFilePresentOnDevice(tapEnv, device,
					BroadBandCommandConstants.FILE_NAME_PARODUS_ENABLE);

			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL : PARODUS_ENABLE file is not present in Arm console");
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNum++;
			stepNumber = "S" + stepNum;
			status = false;
			errorMessage = "PARODUS_ENABLE File is present in device under ATOM console";
			LOGGER.info("***********************************************************");
			LOGGER.info(
					"STEP " + stepNum + ": DESCRIPTION : Verify PARODUS_ENABLE file does not exist in ATOM console");
			LOGGER.info("STEP " + stepNum + ": ACTION : Execute command:find / -iname PARODUS_ENABLE");
			LOGGER.info("STEP " + stepNum + ": EXPECTED : Response should not contain the PARODUS_ENABLE File");
			LOGGER.info("***********************************************************");

			if (isAtomSyncDevice) {
				status = !BroadBandSystemUtils.verifyFileAvailabilityInAtomConsole(tapEnv, device,
						BroadBandCommandConstants.FILE_NAME_PARODUS_ENABLE);
				if (status) {
					LOGGER.info("STEP " + stepNum + ": ACTUAL : PARODUS_ENABLE file is not present in Atom console");
				} else {
					LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);

				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			} else {
				errorMessage = "ATOM console is not available for this device model";
				LOGGER.info("STEP " + stepNum + ": " + errorMessage);
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying Integrate parodus2ccsp with Yocto" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, false, errorMessage,
					false);
		}
	}

	/**
	 *
	 * Test Case # 1: Verify Enable Parodus in log messages
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1: Perform reboot operation</li>
	 * <li>PRE-CONDITION 2: Add Temporary File In Nvram</li>
	 * <li>STEP 1: Verify the parodus is enabled in device</li>
	 * <li>STEP 2: Verify server is connected over SSL in PARODUS log file</li>
	 * <li>STEP 3: Verify init for parodus success in client webpa log file</li>
	 * <li>STEP 4: Verify webpa boot time is logged to WEBPAlog.txt.0 file</li>
	 * <li>STEP 5: Verify webpa Get request is working</li>
	 * <li>STEP 6: Verify webpa Set request is working</li>
	 * <li>STEP 7: Verify harvester is sending reports to parodus</li>
	 * <li>STEP 8: Verify Lmlite is sending reports to parodus</li>
	 * <li>STEP 9: Verify serial number through dmcli command and
	 * /tmp/parodusCmd.cmd</li>
	 * <li>STEP 10: Verify manufacturer through dmcli command and
	 * /tmp/parodusCmd.cmd</li>
	 * <li>STEP 11: Verify last reboot reason through dmcli command and
	 * /tmp/parodusCmd.cmd</li>
	 * <li>STEP 12: Check /tmp/parodusCmd.cmd contains the correct boot-time</li>
	 * <li>STEP 13: Check /tmp/parodusCmd.cmd contains --boot-time-retry-wait
	 * value</li>
	 * <li>STEP 14: Verify Parodus is up and running</li>
	 * <li>STEP 15: Verify WebPA request is success</li>
	 * <li>STEP 16: Kill parodus log and wait for the selfheal to restart
	 * parodus</li>
	 * <li>STEP 17: Check boot time value in /tmp/parodusCmd.cmd from step 1</li>
	 * <li>POST-CONDITION 1: Check if Parodus process is UP and running</li>
	 * <li>POST-CONDITION 2: Verify WebPA request is success</li>
	 * <li>POST-CONDITION 3: Delete Temporary File from NVRAM</li>
	 * <li>POST CONDITION 4. Verify radio status reverted to It's previous
	 * value</li>
	 * </ol>
	 *
	 * @author Sumathi Gunasekaran
	 * @author Praveenkumar Paneerselvam
	 * @Refactor Athira
	 * 
	 * @param device {@link Dut}
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.SYSTEM })
	@TestDetails(testUID = "TC-RDKB-PARODUS-ENBL-1003")
	public void testVerifyEnableParodus(Dut device) {

		// variable to store errorMessage
		String errorMessage = null;
		// variable to store testcaseID
		String testCaseId = "TC-RDKB-PARODUS-ENBL-003";
		// variable to store teststepNumber
		String testStepNumber = "s1";
		// variable to store status
		boolean status = false;

		boolean isBusinessClass = false;
		boolean isDSL = false;
		boolean isWiFiDsiabled = false;

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-ENBL-1003");
		LOGGER.info("TEST DESCRIPTION: Verify Boot time of Parodus");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE CONDITION 1. reboot the device");
		LOGGER.info("PRE CONDITION 2. Add Temporary File In Nvram");
		LOGGER.info("1. Verify webpa boot time is logged to BootTime.log");
		LOGGER.info("2. Verify the parodus is enabled in device");
		LOGGER.info("3. Verify server is connected over SSL in PARODUS log file");
		LOGGER.info("4. Verify init for parodus success in client webpa log file");
		LOGGER.info("5. Verify webpa Get request is working");
		LOGGER.info("6. Verify webpa Set request is working");
		LOGGER.info("7. Verify harvester is sending reports to parodus");
		LOGGER.info("8. Verify Lmlite is sending reports to parodus");
		LOGGER.info("9. Verify serial number through dmcli command and /tmp/parodusCmd.cmd");
		LOGGER.info("10. Verify manufacturer through dmcli command and /tmp/parodusCmd.cmd");
		LOGGER.info("11. Verify last reboot reason through dmcli command and /tmp/parodusCmd.cmd");
		LOGGER.info("12. Check /tmp/parodusCmd.cmd contains the correct boot-time");
		LOGGER.info("13. Check /tmp/parodusCmd.cmd contains --boot-time-retry-wait value");
		LOGGER.info("14. Verify Parodus is up and running");
		LOGGER.info("15. Verify WebPA request is success");
		LOGGER.info("16. Kill parodus log and wait for the selfheal to restart parodus");
		LOGGER.info("17. Check boot time value in /tmp/parodusCmd.cmd from step 1");
		LOGGER.info("POST CONDITION 1. Check if Parodus process is UP and running");
		LOGGER.info("POST CONDITION 2. Verify WebPA request is success");
		LOGGER.info("POST CONDITION 3. Delete Temporary File from NVRAM");
		LOGGER.info("POST CONDITION 4. Verify radio status reverted to It's previous value");
		LOGGER.info("#######################################################################################");

		try {
			isBusinessClass = DeviceModeHandler.isBusinessClassDevice(device);
			isDSL = DeviceModeHandler.isDSLDevice(device);
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");

			/**
			 * PRE-CONDITION 1 : Going to reboot the device
			 */
			LOGGER.info("##########################################################################");
			LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Going to reboot the device.");
			LOGGER.info("PRE-CONDITION 1 : ACTION : Execute command: reboot");
			LOGGER.info("PRE-CONDITION 1 : EXPECTED: Device should come up after reboot");
			LOGGER.info("##########################################################################");
			errorMessage = "Unable to perform device reboot";
			status = BroadBandCommonUtils.rebootAndWaitForStbAccessible(device, tapEnv);
			if (status) {
				LOGGER.info("PRE-CONDITION 1 : ACTUAL : Device reboot successful");
			} else {
				LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 FAILED : " + errorMessage);
			}

			/**
			 * PRE-CONDITION 2 : Add Temporary File In Nvram
			 */
			LOGGER.info("##########################################################################");
			LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Add Temporary File In Nvram");
			LOGGER.info("PRE-CONDITION 2 : ACTION : EXECUTE COMMAND rm <filename>");
			LOGGER.info("PRE-CONDITION 2 : EXPECTED : File Should be added Successfully");
			LOGGER.info("##########################################################################");
			errorMessage = "Failed to add temporary file in nvram";
			int totalNoOfFiles = 0;

			ArrayList<String> mustHaveLogFileList = new ArrayList<String>();

			if (!isBusinessClass) {
				mustHaveLogFileList.add(BroadBandTestConstants.FILE_NAME_HARVESTER);
			}
			mustHaveLogFileList.add(BroadBandTestConstants.FILE_NAME_LM);
			mustHaveLogFileList.add(BroadBandTestConstants.RDKLOGS_LOGS_WEBPA_TXT_0
					.replaceAll(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING));
			mustHaveLogFileList.add(BroadBandTestConstants.STRING_PARODUS_LOG);
			totalNoOfFiles = mustHaveLogFileList.size();
			Map<String, String> mapForLogFileWithPath = BroadBandCommonUtils
					.verifyLogAlbltyAndTailLogToNvramInGivenConsole(device, tapEnv, mustHaveLogFileList,
							CommonMethods.concatStringUsingStringBuffer(
									String.valueOf(BroadBandTestConstants.SYMBOL_PLUS),
									String.valueOf(BroadBandTestConstants.CONSTANT_1)),
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
							BroadBandTestConstants.TWENTYFIVE_MINUTES_IN_MILLS, BroadBandTestConstants.ARM);
			status = (totalNoOfFiles == mapForLogFileWithPath.size());
			if (status) {
				LOGGER.info("PRE-CONDITION 2 : ACTUAL : File added in nvram Successfully");
			} else {
				LOGGER.error("PRE-CONDITION 2 : ACTUAL : " + errorMessage);
				throw new TestException(
						BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1: Verify webpa boot time is logged to WEBPAlog.txt.0 file
			 */
			testStepNumber = "s1";
			status = false;
			errorMessage = CommonUtils.concatStringUsingStringBuffer("Unable to verify webpa boot time:",
					BroadBandTraceConstants.WEBPA_READY_UPTIME, "in log file",
					BroadBandCommandConstants.LOG_FILE_WEBPA);
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1 : DESCRIPTION:  Verify  webpa boot time is logged to BootTime.log file");
			LOGGER.info(
					"STEP 1 : ACTION:  Execute command:grep -i boot_to_WEBPA_READY_uptime /rdklogs/logs/BootTime.log");
			LOGGER.info("STEP 1 : EXPECTED : Log message should be present in BootTime.log");
			LOGGER.info("**********************************************************************************");
			/*
			 * Observed a possibiliy that there might be a delay in log getting updated in
			 * "/rdklogs/logs/WEBPAlog.txt.0" log file. So retry for 3 minutes with a 30
			 * seconds interval
			 */
			errorMessage = "Failed to get the log message WEBPA_READY in BootTime.log";
			String response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_WEBPA_READY, BroadBandCommandConstants.FILE_BOOT_TIME_LOG,
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP 1 : ACTUAL : Successfully verified webpa boot time from BootTime.log file");
			} else {
				LOGGER.error("STEP 1 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 2: Verify the parodus is enabled in device
			 */
			testStepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2 : DESCRIPTION:  Verify the parodus is enabled in device");
			LOGGER.info("STEP 2 : ACTION:  Execute command:ps | grep -i parodus");
			LOGGER.info("STEP 2 : EXPECTED : Log message should be present in Parodus");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to get the status of Parodus using command:"
					+ BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS;
			status = CommonMethods.isNotNull(
					tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS));
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfully verified the status of parodus as enabled");
			} else {
				LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 3: Verify server is connected over SSL in PARODUS log file
			 */
			testStepNumber = "s3";
			status = false;
			errorMessage = CommonUtils.concatStringUsingStringBuffer(
					"Unable to verify server connection from log file:", BroadBandCommandConstants.LOG_FILE_PARODUS,
					"with log message", BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION);
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3 : DESCRIPTION:  Verify server is connected over SSL in PARODUS log file");
			LOGGER.info(
					"STEP 3 : ACTION:  Execute command:ps | grep -i Connected to server over SSL /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 3 : EXPECTED : Log message should be present in PARODUSlog.txt");
			LOGGER.info("**********************************************************************************");
			status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION,
					mapForLogFileWithPath.get(BroadBandTestConstants.STRING_PARODUS_LOG),
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 3 : ACTUAL : Server Connection is verified successfully from parodus log file");
			} else {
				LOGGER.error("STEP 3 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 4: Verify init for parodus success in client webpa log file
			 */
			testStepNumber = "s4";
			status = false;
			errorMessage = "Unable to verify init for parodus success:"
					+ BroadBandTestConstants.RDKLOGS_LOGS_WEBPA_TXT_0;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4 : DESCRIPTION:  Verify init for parodus success in client webpa log file");
			LOGGER.info(
					"STEP 4 : ACTION:  Execute command:ps | grep -i Init for parodus Success /rdklogs/logs/WEBPAlog.txt.0");
			LOGGER.info("STEP 4 : EXPECTED : Log message should be present in WEBPAlog.txt.0");
			LOGGER.info("**********************************************************************************");
			/*
			 * That there might be a delay in log getting updated in
			 * /rdklogs/logs/WEBPAlog.txt.0" log file. So retry for 3 minutes with a 30
			 * seconds interval
			 */
			status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INTEGRATION,
					mapForLogFileWithPath.get(BroadBandTestConstants.RDKLOGS_LOGS_WEBPA_TXT_0
							.replaceAll(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING)),
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 4 : ACTUAL : Init for parodus success is verified successfully from webpa log file");
			} else {
				LOGGER.error("STEP 4 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 5: verify webpa Get request is working
			 */
			testStepNumber = "s5";
			status = false;
			errorMessage = "Failed to get the webpa request for parameter:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_WEBPA_VERSION;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5 : DESCRIPTION:  verify webpa Get request is working");
			LOGGER.info(
					"STEP 5 : ACTION:  Execute command:dmcli eRT getv Device.Device.X_RDKCENTRAL-COM_Webpa.Version");
			LOGGER.info("STEP 5 : EXPECTED : The webpa command should Execute successfully");
			LOGGER.info("**********************************************************************************");
			status = CommonUtils.isNotEmptyOrNull(
					tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WEBPA_VERSION));
			if (status) {
				LOGGER.info("STEP 5 : ACTUAL : Successfully verified get request through webpa command");
			} else {
				LOGGER.error("STEP 5 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 6: Verify webpa Set request is working
			 */
			testStepNumber = "s6";
			status = false;
			errorMessage = CommonUtils.concatStringUsingStringBuffer("Failed to Set value for WebPa parameter:",
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, "with Value:",
					BroadBandTestConstants.FALSE);
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6 : DESCRIPTION:  Verify webpa Set request is working");
			LOGGER.info("STEP 6 : ACTION:  Execute command:dmcli eRT setv Device.Device.WiFi.Radio.10000.Enable");
			LOGGER.info("STEP 6 : EXPECTED : The webpa command should Execute successfully");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE);
			if (status) {
				isWiFiDsiabled = true;
				LOGGER.info("STEP 6 : ACTUAL : Successfully verified set request through webpa command");
			} else {
				LOGGER.error("STEP 6 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 7: verify harvester is sending reports to parodus
			 */
			testStepNumber = "s7";
			status = false;
			errorMessage = "Unable to verify reports sending to parodus from log file:"
					+ BroadBandCommandConstants.FILE_HARVESTER_LOG;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7 : DESCRIPTION: verify harvester is sending reports to parodus");
			LOGGER.info(
					"STEP 7 : ACTION:  Execute command:grep -i Init for parodus Success /rdklogs/logs/HarvesterLog.txt.0");
			LOGGER.info("STEP 7 : EXPECTED : Log message should be present in Harvesterlog.txt.0");
			LOGGER.info("**********************************************************************************");
			if (isBusinessClass) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 7 is not applicable for Bussiness class device.");
				LOGGER.info("**********************************************************************************");
				LOGGER.error("Bussiness class device doesn't have harvester");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						"Bussiness class device doesn't have harvester", false);
			} else {
				status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INTEGRATION,
						mapForLogFileWithPath.get(BroadBandTestConstants.FILE_NAME_HARVESTER)));
				if (status) {
					LOGGER.info(
							"STEP 7 : ACTUAL : Successfully verified reports sending to parodus from harvester log file");
				} else {
					LOGGER.error("STEP 7 : ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			}

			/**
			 * STEP 8: verify Lmlite is sending reports to parodus
			 */
			testStepNumber = "s8";
			status = false;
			errorMessage = "Unable to verify reports sending to parodus from log file:"
					+ BroadBandCommandConstants.LOG_FILE_LMLITE;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8 : DESCRIPTION: verify Lmlite is sending reports to parodus");
			LOGGER.info(
					"STEP 8 : ACTION:  Execute command:grep -i Init for parodus Success /rdklogs/logs/Lmlite.txt.0");
			LOGGER.info("STEP 8 : EXPECTED : Log message should be present in Lmlite.txt.0");
			LOGGER.info("**********************************************************************************");
			status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INTEGRATION,
					mapForLogFileWithPath.get(BroadBandTestConstants.FILE_NAME_LM)));
			if (status) {
				LOGGER.info("STEP 8 : ACTUAL : Successfully verified reports sending to parodus from Lmlite log file");
			} else {
				LOGGER.error("STEP 8 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 9: verify serial number through dmcli command and tmp/parodusCmd.cmd
			 */
			testStepNumber = "s9";
			status = false;
			errorMessage = CommonUtils.concatStringUsingStringBuffer("Failed to verify: ",
					TR69ParamConstants.TR69_PARAM_SERIAL_NUMBER, "through dmcli and parodusCmd.cmd");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9 : DESCRIPTION:  verify serial number through dmcli command and  /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 9 : ACTION:  Execute command:dmcli eRT getv Device.DeviceInfo.SerialNumber");
			LOGGER.info("STEP 9 : EXPECTED : Dmcli command output and grep command should have the same serial number");
			LOGGER.info("**********************************************************************************");
			status = ParodusUtils.verifyParodusAndDmcliCommandResponse(tapEnv, device,
					TR69ParamConstants.TR69_PARAM_SERIAL_NUMBER);
			if (status) {
				LOGGER.info(
						"STEP 9 : ACTUAL : Successfully verified Device.DeviceInfo.SerialNumber and grep command has same serial number");
			} else {
				LOGGER.error("STEP 9 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 10:verify manufacturer through dmcli command and /tmp/parodusCmd.cmd
			 */
			testStepNumber = "s10";
			status = false;
			errorMessage = CommonUtils.concatStringUsingStringBuffer("Failed to verify: ",
					BroadBandWebPaConstants.WEBPA_DEVICE_MANUFACTURER_NAME, "through dmcli and parodusCmd.cmd");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10 : DESCRIPTION:  verify manufacturer through dmcli command and /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 10 : ACTION:  Execute command:dmcli eRT getv Device.DeviceInfo.Manufacturer");
			LOGGER.info("STEP 10 : EXPECTED : Dmcli command output and grep command should have the same manufacturer");
			LOGGER.info("**********************************************************************************");
			status = ParodusUtils.verifyParodusAndDmcliCommandResponse(tapEnv, device,
					BroadBandWebPaConstants.WEBPA_DEVICE_MANUFACTURER_NAME);
			if (status) {
				LOGGER.info(
						"STEP 10 : ACTUAL : Successfully verified Device.DeviceInfo.Manufacturer and grep command has same serial number");
			} else {
				LOGGER.error("STEP 10 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 11:verify last reboot reason through dmcli command and
			 * /tmp/parodusCmd.cmd
			 */
			testStepNumber = "s11";
			status = false;
			errorMessage = CommonUtils.concatStringUsingStringBuffer("Failed to verify: ",
					BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON, "through dmcli and parodusCmd.cmd");
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11 : DESCRIPTION:  verify last reboot reason through dmcli command and /tmp/parodusCmd.cmd");
			LOGGER.info(
					"STEP 11 : ACTION:  Execute command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
			LOGGER.info(
					"STEP 11 : EXPECTED : Dmcli command output and grep command should have the same reboot reason");

			LOGGER.info("**********************************************************************************");
			status = ParodusUtils.verifyParodusAndDmcliCommandResponse(tapEnv, device,
					BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
			if (status) {
				LOGGER.info(
						"STEP 11 : ACTUAL : Successfully verified DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason and grep command has same serial number");
			} else {
				LOGGER.error("STEP 11 : ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 12: Check /tmp/parodusCmd.cmd contains the correct boot-time
			 */
			LOGGER.info("**********************************************************************************");
			testStepNumber = "s12";
			errorMessage = "Verification of boot time failed. Boot time not in range or invalid.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Check /tmp/parodusCmd.cmd contains the correct boot-time");
			LOGGER.info("STEP 12: ACTION :  Execute command /tmp/parodusCmd.cmd and "
					+ "1. Device start time should before than boot time. Device start time = current time - uptime in sec (Device.DeviceInfo.UpTime)"
					+ " 2.Boot time should be with-in the range 0 - 4294967295");
			LOGGER.info(
					"STEP 12: EXPECTED : Boot time should be with in range 0 - 4294967295 and boot time should be greater than device start up time");
			LOGGER.info("**********************************************************************************");
			String bootTime = null;
			if (isDSL) {
				errorMessage = "Boot time validation is not applicable for DSL devices";
				LOGGER.error("STEP 12: ACTUAL : " + "Boot time validation is not applicable for DSL devices");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			} else {
				bootTime = ParodusUtils.getParodusBootTime(tapEnv, device);
				LOGGER.info("Boot time value is - " + bootTime);
				errorMessage = "Failed to get Boot time value from /tmp/parodusCmd.cmd";
				if (CommonMethods.isNotNull(bootTime)) {
					BroadBandResultObject result = ParodusUtils.verifyParodusBootTimeIsValid(tapEnv, device, bootTime);
					status = result.isStatus();
					errorMessage = result.getErrorMessage();
				}
				if (status) {
					LOGGER.info("STEP 12: ACTUAL : Boot time value in the /tmp/parodusCmd.cmd is present.");
				} else {
					LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
			}
			/**
			 * STEP 13: Check /tmp/parodusCmd.cmd contains --boot-time-retry-wait value
			 */
			testStepNumber = "s13";
			errorMessage = "--boot-time-retry-wait  is not present";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Check /tmp/parodusCmd.cmd contains --boot-time-retry-wait value");
			LOGGER.info(
					"STEP 13: ACTION : 1. Execute command /tmp/parodusCmd.cmd and verify boot time retry wait value is present");
			LOGGER.info("STEP 13: EXPECTED : --boot-time-retry-wait should be present");
			LOGGER.info("**********************************************************************************");
			status = ParodusUtils.verifyParodusBootRetryCountIsPresent(tapEnv, device);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : -boot-time-retry-wait value is present in /tmp/parodusCmd.cmd");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			LOGGER.info("**********************************************************************************");

			/**
			 * STEP 14: Verify Parodus is up and running
			 */
			testStepNumber = "s14";
			errorMessage = "Failed to get processor id for parodus. Parodus process is not up.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Verify Parodus is up and running");
			LOGGER.info("STEP 14: ACTION : Execute command pidof parodus");
			LOGGER.info("STEP 14: EXPECTED : pidof parodus should be returned");
			LOGGER.info("**********************************************************************************");
			String processId = CommonUtils.getPidOfProcess(device, tapEnv, StbProcess.PARODUS.getProcessName());
			status = CommonMethods.isNotNull(processId);
			if (status) {
				LOGGER.info("STEP 14: ACTUAL :Valid Process Id is obtained, process Id is - " + processId);
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 15: Verify WebPA request is success
			 */
			testStepNumber = "s15";
			errorMessage = "Failed to execute WebPA request.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION : Verify WebPA request is success");
			LOGGER.info("STEP 15: ACTION : Execute WebPA get request for the parameter Device.DeviceInfo.UpTime");
			LOGGER.info("STEP 15: EXPECTED : WebPA request should be success and value should be returned");
			LOGGER.info("**********************************************************************************");
			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_UPTIME);
			status = CommonMethods.isNotNull(response);
			if (status) {
				LOGGER.info("STEP 15: ACTUAL : WebPA request is success and value returned is " + response);
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			/**
			 * STEP 16: Kill parodus log and wait for the selfheal to restart parodus
			 */
			testStepNumber = "s16";
			errorMessage = "Failed to restart Parodus processor with in 15 min";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION : Kill parodus log and wait for the selfheal to restart parodus");
			LOGGER.info(
					"STEP 16: ACTION : Execute below commands : 1. Kill -11 <parodus pid>2. Wait 15 min for Self Heal to start the parodus processor"
							+ "3. Verify with command pidof parodus  ");
			LOGGER.info("STEP 16: EXPECTED : Parodus should get restarted successfully with in 15 min.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandSelfHealUtils.initiateProcessCrashAndVerifyProcessRestartedStatus(device, tapEnv,
					StbProcess.PARODUS);
			if (status) {
				LOGGER.info("STEP 16: ACTUAL : Parodus successfully restarted by selfheal");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

			/**
			 * STEP 17: Check boot time value in /tmp/parodusCmd.cmd from step 13
			 */
			testStepNumber = "s17";
			errorMessage = "Boot time got changed.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 17: DESCRIPTION : Check boot time value in /tmp/parodusCmd.cmd from step 13");
			LOGGER.info(
					"STEP 17: ACTION : Execute command /tmp/parodusCmd.cmd and verify the boot time value from step 13");
			LOGGER.info("STEP 17: EXPECTED : Both boot time should be the same");
			LOGGER.info("**********************************************************************************");
			if (isDSL) {
				errorMessage = "Boot time validation is not applicable for DSL devices";
				LOGGER.error("STEP 17: ACTUAL : " + "Boot time validation is not applicable for DSL devices");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			} else {
				bootTime = ParodusUtils.getParodusBootTime(tapEnv, device);
				LOGGER.info("Boot time value is - " + bootTime);
				errorMessage = "Failed to get Boot time value from /tmp/parodusCmd.cmd";
				if (CommonMethods.isNotNull(bootTime)) {
					BroadBandResultObject result = ParodusUtils.verifyParodusBootTimeIsValid(tapEnv, device, bootTime);
					status = result.isStatus();
					errorMessage = result.getErrorMessage();
				}
				if (status) {
					LOGGER.info("STEP 17: ACTUAL : Boot time value is present in /tmp/parodusCmd.cmd");
				} else {
					LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
			}
		} catch (Exception e) {
			status = false;
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
					errorMessage, true);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST-CONDITION 1: Check if Parodus process is UP and running
			 */
			status = false;
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION : Check if Parodus process is UP and running");
			LOGGER.info("POST-CONDITION 1: ACTION : Execute command pidof parodus");
			LOGGER.info(
					"POST-CONDITION 1: EXPECTED : Parodus process should be UP and running, If Parodus processor is not running restart the device and verify");
			LOGGER.info("##########################################################################");
			errorMessage = "Unable to verify the Parodus process UP and running";
			String processId = CommonUtils.getPidOfProcess(device, tapEnv, StbProcess.PARODUS.getProcessName());
			status = CommonMethods.isNotNull(processId);
			if (!status) {
				if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
					processId = CommonUtils.getPidOfProcess(device, tapEnv, StbProcess.PARODUS.getProcessName());
					status = CommonMethods.isNotNull(processId);
				}
			}
			if (status) {
				LOGGER.info("POST-CONDITION 1: ACTUAL : Successfully verified Parodus process is UP and running");
			} else {
				LOGGER.error("POST-CONDITION 1: ACTUAL : " + errorMessage);
			}
			/**
			 * POST-CONDITION 2: Verify WebPA request is success
			 */
			status = false;
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 2: DESCRIPTION : Verify WebPA request is success");
			LOGGER.info(
					"POST-CONDITION 2: ACTION : Execute WebPA get request for the parameter Device.DeviceInfo.UpTime");
			LOGGER.info("POST-CONDITION 2: EXPECTED : WebPA request should be success and value should be returned");
			LOGGER.info("##########################################################################");
			errorMessage = "Failed to verify WebPA request";
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("POST-CONDITION 2: ACTUAL : Successfully verified Parodus process is UP and running");
			} else {
				LOGGER.error("POST-CONDITION 2: ACTUAL : " + errorMessage);
			}
			/**
			 * POST-CONDITION 3: Delete Temporary File from NVRAM
			 */
			status = false;
			LOGGER.info("##########################################################################");
			LOGGER.info("POST-CONDITION 3: DESCRIPTION : Delete Temporary File from NVRAM");
			LOGGER.info("POST-CONDITION 3: ACTION : Execute Command rm <filename>");
			LOGGER.info("POST-CONDITION 3: EXPECTED : File Should be removed Successfully");
			LOGGER.info("##########################################################################");
			errorMessage = "Failed to delete temporary file from NVRAM";
			List<String> logFileList = new ArrayList<>();
			logFileList.add(BroadBandTestConstants.FILE_NAME_HARVESTER);
			logFileList.add(BroadBandTestConstants.FILE_NAME_LM);
			logFileList.add(BroadBandTestConstants.RDKLOGS_LOGS_WEBPA_TXT_0
					.replaceAll(BroadBandCommandConstants.DIRECTORY_LOGS, BroadBandTestConstants.EMPTY_STRING));
			logFileList.add(BroadBandTestConstants.STRING_PARODUS_LOG);
			status = BroadBandCommonUtils.deleteTemporaryFilesInNvram(device, tapEnv, logFileList);
			if (status) {
				LOGGER.info("POST-CONDITION 3: ACTUAL : Successfully deleted the Temporary File from NVRAM");
			} else {
				LOGGER.error("POST-CONDITION 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("Status of Disabled ? : " + isWiFiDsiabled);
			if (isWiFiDsiabled) {
				/**
				 * POST-CONDITION 4: Verify radio status reverted to It's previous value
				 */
				status = false;
				LOGGER.info("##########################################################################");
				LOGGER.info("POST-CONDITION 4: DESCRIPTION :Verify radio status reverted to It's previous value");
				LOGGER.info(
						"POST-CONDITION 4: ACTION : Execute command:dmcli eRT setv Device.Device.WiFi.Radio.10000.Enable");
				LOGGER.info("POST-CONDITION 4: EXPECTED : The webpa command should Execute successfully");
				LOGGER.info("##########################################################################");
				errorMessage = "Failed to revert the radio status to it's previous value";
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE);
				if (status) {
					LOGGER.info(
							"POST-CONDITION 4: ACTUAL : Successfully reverted the radio status to It's previous value");
				} else {
					LOGGER.error("POST-CONDITION 4: ACTUAL : " + errorMessage);
				}

				LOGGER.info("POST CONDITION 4. Verify radio status reverted to It's previous value");
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
	}

	/**
	 * Helper method to search text in webpa log file either in ARM or ATOM console
	 * 
	 * @param device       {@link Dut}
	 * @param searchText   search text message
	 * @param isAtomDevice is Atom device
	 * @return status
	 * @author ArunKumar Jayachandran
	 * @refactor Govardhan
	 */
	private static boolean searchWebpaLogInArmOrAtom(Dut device, String searchText, boolean isAtomDevice) {
		LOGGER.debug("STARTING METHOD searchWebpaLogInArmOrAtom");
		String response = null;
		boolean status = false;
		try {
			response = isAtomDevice
					? BroadBandCommonUtils.searchLogFilesInAtomConsoleByPolling(tapEnv, device, searchText,
							BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
					: BroadBandCommonUtils.searchLogFiles(tapEnv, device, searchText,
							BroadBandCommandConstants.LOG_FILE_WEBPA, BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			status = CommonMethods.isNotNull(response);
			if (!status) {
				LOGGER.info("Validating device trace if logs are unavailable in "
						+ BroadBandCommandConstants.LOG_FILE_WEBPA);
				response = tapEnv.searchAndGetTraceLineWithMatchingString(device, searchText,
						BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				status = CommonMethods.isNotNull(response);

			}
		} catch (Exception e) {
			LOGGER.error("Exception occured while validating logs in rdklogs/device trace " + e.getMessage());
		}
		LOGGER.debug("ENDING METHOD searchWebpaLogInArmOrAtom");
		return status;
	}

	/**
	 * Test to verify configurable parodus reboot reason
	 * <ol>
	 * <li>Kill parodus process and verify no pid is obtained</li>
	 * <li>Append parodusCmd with close reason file</li>
	 * <li>Write custom reboot reason for parodus in file
	 * /tmp/parodus_close_reason.txt</li>
	 * <li>Start parodus using modified parodusCmd</li>
	 * <li>Wait for parodus connection and ping received log message</li>
	 * <li>Tail paroduslog to nvram and reboot the device using webpa</li>
	 * <li>Verify Parodus SIGTERM received in PARODUSlog</li>
	 * <li>Verify Shutdown reason 'Closing from file' in PARODUSlog</li>
	 * <li>Verify configured close reason present in PARODUSlog</li>
	 * <li>Verify cloud status set offline in PARODUSlog</li>
	 * </ol>
	 * 
	 * @author Ashwin sankara
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PARODUS-1009")
	public void testVerifyConfigurableParodusRebootReason(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PARODUS-009";
		String stepNum = "s1";
		String errorMessage = null;
		String processId = null;
		boolean status = false;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1009");
		LOGGER.info("TEST DESCRIPTION: Test to verify configurable parodus reboot reason");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Kill parodus process and verify no pid is obtained");
		LOGGER.info("2. Append parodusCmd with close reason file");
		LOGGER.info("3. Write custom reboot reason for parodus in file /tmp/parodus_close_reason.txt");
		LOGGER.info("4. Start parodus using modified parodusCmd");
		LOGGER.info("5. Wait for parodus connection and ping received log message");
		LOGGER.info("6. Tail paroduslog to nvram and reboot the device using webpa");
		LOGGER.info("7. Verify Parodus SIGTERM received in PARODUSlog");
		LOGGER.info("8. Verify Shutdown reason 'Closing from file' in PARODUSlog");
		LOGGER.info("9. Verify configured close reason present in PARODUSlog");
		LOGGER.info("10. Verify cloud status set offline in PARODUSlog");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Unable to kill parodus process";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Kill parodus process and verify no pid is obtained");
			LOGGER.info(
					"STEP 1: ACTION : Execute commands: 1. kill -9 parodus (for AtomSyncDevices) or systemctl stop parodus.serice (other devices) \n2. pidof parodus");
			LOGGER.info("STEP 1: EXPECTED : Parodus process is killed and no pid is obtained");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				processId = CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS);
				CommonUtils.killTheProcessWithPid(device, tapEnv, processId);
			} else {
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_SYSTEMCTL_STOP_PARODUS);
			}
			status = CommonMethods
					.isNull(CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS));

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Parodus process is killed and no pid is obtained");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to update close reason file parameter in /tmp/parodusCmd.cmd";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Append parodusCmd with close reason file");
			LOGGER.info(
					"STEP 2: ACTION : Execute commands: 1. sed -i \"s#&#--close-reason-file=/tmp/parodus_close_reason.txt#g\" /tmp/parodusCmd.cmd \n2. grep -i \"--close-reason-file=/tmp/parodus_close_reason.txt\" /tmp/parodusCmd.cmd");
			LOGGER.info("STEP 2: EXPECTED : Close reason file parameter is added to parodusCmd");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_SED_ADD_PARODUS_CLOSE_REASON_FILE);
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandCommandConstants.CMD_TEMP_PARODUS_CLOSE_REASON,
					BroadBandCommandConstants.FILE_PATH_PARODUS_CMD));

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Close reason file parameter is added to parodusCmd");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Failed to update custom close reason in /tmp/parodus_close_reason.txt";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION : Write custom reboot reason for parodus in file /tmp/parodus_close_reason.txt");
			LOGGER.info(
					"STEP 3: ACTION : Execute commands: 1. echo \"Closing from file\" > /tmp/parodus_close_reason.txt \n2. grep -i \"Closing from file\" /tmp/parodus_close_reason.txt");
			LOGGER.info("STEP 3: EXPECTED : Custom reboot reason is written into parodus_close_reason.txt file");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_ECHO_CUSTOM_PARODUS_REASON_INTO_FILE);
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CLOSING_FROM_FILE,
					BroadBandCommandConstants.CMD_TEMP_PARODUS_CLOSE_REASON));

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Custom reboot reason is written into parodus_close_reason.txt file");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to clear PARODUSlog.txt.0";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Start parodus using modified parodusCmd");
			LOGGER.info("STEP 4: ACTION : Execute commands: 1.`cat /tmp/parodusCmd.cmd` & \n2. pidof parodus");
			LOGGER.info("STEP 4: EXPECTED : Obtained pid for parodus after starting");
			LOGGER.info("**********************************************************************************");

			if (CommonUtils.clearLogContents(device, tapEnv, BroadBandCommandConstants.LOG_FILE_PARODUS)) {
				errorMessage = "Failed to start parodus process using custom /tmp/parodusCmd.cmd";
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_START_CUSTOM_PARODUS_FROM_TMPCMD);
				status = CommonMethods.isNotNull(
						CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS));
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Obtained pid for parodus after starting");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Failed to find PARODUS close reason file log message present after restarting with option";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Wait for parodus connection and ping received log message");
			LOGGER.info(
					"STEP 5: ACTION : Execute commands: 1. grep \"PARODUS: sigterm_close_reason_file is /tmp/parodus_close_reason.txt\" /rdklogs/logs/PARODUSlog.txt.0 \n2. grep \"PARODUS: Ping received with payload\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 5: EXPECTED : Log message is present for parodus ping received after connection");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CLOSE_REASON_FILE,
					BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
				errorMessage = "Failed to find PARODUS ping received log message after custom start";
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_SESHAT_INTEGRATION_COMPLETE,
						BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			}

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Log message is present for parodus ping received after connection");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Failed to set Device.X_CISCO_COM_DeviceControl.RebootDevice with string Device";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Tail paroduslog to nvram and reboot the device using webpa");
			LOGGER.info(
					"STEP 6: ACTION : Execute commands:1. tail -f /rdklogs/logs/PARODUSlog.txt.0 > /nvram/PARODUStail &2. dmcli eRT setv Device.X_CISCO_COM_DeviceControl.RebootDevice string Device");
			LOGGER.info("STEP 6: EXPECTED : Device rebooted successfully");
			LOGGER.info("**********************************************************************************");

			tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PARODUSLOGS_NVRAM);
			if (BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CONTROL_DEVICE_REBOOT, BroadBandTestConstants.DEVICE,
					BroadBandTestConstants.CONSTANT_0)) {
				errorMessage = "Device did not go for reboot after webpa reboot set";
				if (CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
						BroadBandTestConstants.CONSTANT_6)) {
					errorMessage = "Device did not come up after webpa reboot";
					status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
				}
			}

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Device rebooted successfully");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			errorMessage = "Failed to find Parodus SIGTERM received log message";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify Parodus SIGTERM received in PARODUSlog");
			LOGGER.info("STEP 7: ACTION : Execute command: grep -i \"PARODUS: SIGTERM received\" /nvram/PARODUStail");
			LOGGER.info("STEP 7: EXPECTED : Parodus SIGTERM received is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_SIGTERM_RECEIVED,
					BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Parodus SIGTERM received is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Failed to find shutdown reason 'Closing from file' log message";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify Shutdown reason 'Closing from file' in PARODUSlog");
			LOGGER.info(
					"STEP 8: ACTION : Execute command: grep -i \"PARODUS: shutdown reason at close Closing from file\" /nvram/PARODUStail");
			LOGGER.info("STEP 8: EXPECTED : Shutdown reason 'Closing from file' is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CUSTOM_SHUTDOWN_REASON,
					BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Shutdown reason 'Closing from file' is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Failed to find configured close reason \"Closing from file\" log message";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Verify configured close reason present in PARODUSlog");
			LOGGER.info(
					"STEP 9: ACTION : Execute command: grep -i \"PARODUS: Closed by SIGTERM, reason: Closing from file\" /nvram/PARODUStail");
			LOGGER.info("STEP 9: EXPECTED : Configured close reason is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_CUSTOM_CLOSE_REASON,
					BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Configured close reason is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "Failed to find cloud status set offline log message in PARODUSlog";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify cloud status set offline in PARODUSlog");
			LOGGER.info(
					"STEP 10: ACTION : Execute command: grep -i \"PARODUS: cloud_status set as offline after connection close\" /nvram/PARODUStail");
			LOGGER.info("STEP 10: EXPECTED : Cloud status set offline is present in PARODUSlog");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CLOUD_STATUS_SET_OFFLINE,
					BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL));

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Cloud status set offline is present in PARODUSlog");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove nvram parodus tail file");
			LOGGER.info("POST-CONDITION : ACTION : Execute command: rm -rf /nvram/PARODUStail");
			LOGGER.info("POST-CONDITION : EXPECTED : Post condition executed successsfully");

			status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
					BroadBandCommandConstants.FILE_PATH_NVRAM_PARODUS_TAIL);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-1009");
	}

	/**
	 * <li>1. Verify Webpa process is working fine</li>
	 * <li>2. Verify Connected to server over SSL log message in PARODUS log
	 * file</li>
	 * <li>3. Verify Received temporary redirection response log message in PARODUS
	 * log file</li>
	 * <li>4. Verify full url log message in PARODUS log file</li>
	 * <li>5. Verify startParodus is enabled log message in Arm/Console log
	 * file</li>
	 * <li>6. Get PARODUS boot time value</li>
	 * <li>7. Verify Webpa process is working fine after reconnection</li>
	 * <li>8. Verify parodus connection is IPv4/IPv6 mode</li>
	 * 
	 * @author ArunKumar Jayachandran
	 * @refactor Govardhan
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.WEBPA })
	@TestDetails(testUID = "TC-RDKB-PARODUS-NOPOLL-1001")
	public void testToVerifyParodusNoPoll(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-NOPOLL-1001 ");
		LOGGER.info("TEST DESCRIPTION: Test to verify parodus nopoll ");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify Webpa process is working fine");
		LOGGER.info("2. Verify Connected to server over SSL log message in PARODUS log file");
		LOGGER.info("3. Verify Received temporary redirection response log message in PARODUS log file");
		LOGGER.info("4. Verify full url log message in PARODUS log file");
		LOGGER.info("5. Verify startParodus is enabled log message in Arm/Console log file");
		LOGGER.info("6. Get PARODUS boot time value");
		LOGGER.info("7. Verify Webpa process is working fine after reconnection");
		LOGGER.info("8. Verify parodus connection is IPv4/IPv6 mode");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-PARODUS-NOPOLL-001";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		// time format from console log
		SimpleDateFormat format = new SimpleDateFormat(BroadBandTestConstants.TIMESTAMP_FORMAT_LOG_MESSAGE);
		// long value to store time difference
		long timeDifference = BroadBandTestConstants.CONSTANT_0;
		// console log time from parodus log file
		Date dateTimeParodus = new Date();
		// Console log time from console log file
		Date dateTimeConsole = new Date();
		// variable declaration ends

		try {
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Reboot the device");
			LOGGER.info("PRE-CONDITION : ACTION : Execute command: /sbin/reboot");
			LOGGER.info("PRE-CONDITION : EXPECTED : Device should be accessible after reboot");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL: Device is accessible status after reboot: " + status);
			} else {
				LOGGER.error("PRE-CONDITION: ACTUAL: Device is not accessible after reboot ");
				throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + status);
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			stepNumber = "s1";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Verify Webpa is working fine");
			LOGGER.info("STEP 1: ACTION: Execute webpa get command for Device.DeviceInfo.SerialNumber");
			LOGGER.info("STEP 1: EXPECTED: Webpa should success and should get the response");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.SerialNumber";
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully received serial number from webpa query");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNumber = "s2";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify Connected to server over SSL log message in PARODUS log file");
			LOGGER.info(
					"STEP 2: ACTION: Execute command: grep -i \"Connected to server over SSL\" /rdklogs/logs/PARODUSlog.txt.*");
			LOGGER.info("STEP 2: EXPECTED: Response should contain the log message in PARODUS log file");
			LOGGER.info("******************************************************************************");
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Failed to get log message ",
					BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, " from parodus log file");
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (CommonMethods.isNotNull(response)) {
				// get the time stamp from the response output
				response = CommonMethods.patternFinder(response,
						BroadBandTestConstants.PATTERN_TIME_FORMAT_IN_LOG_FILE);
				if (CommonMethods.isNotNull(response)) {
					// convert the time stamp to formated output
					dateTimeParodus = format.parse(response);
					status = true;
				}
			}
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL: Successfully verified log message in PARODUS log file and time stamp from the response is: "
								+ dateTimeParodus);
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP 3: DESCRIPTION: Verify Received temporary redirection response log message in PARODUS log file");
			LOGGER.info(
					"STEP 3: ACTION: Execute command: grep -i \"Received temporary redirection response message\" /rdklogs/logs/PARODUSlog.txt.*");
			LOGGER.info("STEP 3: EXPECTED: Response should contain the log message in PARODUS log file");
			LOGGER.info("******************************************************************************");
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Failed to get log message ",
					BroadBandTraceConstants.LOG_MESSAGE_RECEIVED_TEMP_REDIRECT, " from parodus log file");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_RECEIVED_TEMP_REDIRECT,
					BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully verified log message in parodus log file");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Verify full url log message in PARODUS log file");
			LOGGER.info("STEP 4: ACTION: Execute command: grep -i \"full url\" /rdklogs/logs/PARODUSlog.txt.*");
			LOGGER.info("STEP 4: EXPECTED: Response should contain the log message in PARODUS log file");
			LOGGER.info("******************************************************************************");
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Failed to get log message ",
					BroadBandTraceConstants.LOG_MESSAGE_FULL_URL, " from parodus log file");
			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_FULL_URL, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 4: ACTUAL: Successfully verified log message in parodus log file");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			stepNumber = "s5";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Verify startParodus is enabled log message in Arm/Console log file");
			LOGGER.info(
					"STEP 5: ACTION: Execute command: grep -i \"startParodus is enabled\" /rdklogs/logs/Arm(or)consolelog.txt.0");
			LOGGER.info(
					"STEP 5: EXPECTED: Response should contain the log message in Armconsole log file. And get the timestamp value");
			LOGGER.info("******************************************************************************");
			errorMessage = BroadBandCommonUtils.concatStringUsingStringBuffer("Failed to get log message ",
					BroadBandTraceConstants.LOG_MESSAGE_START_PARODUS, " from parodus log file");
			String consoleLog = CommonMethods.isAtomSyncAvailable(device, tapEnv)
					? BroadBandCommandConstants.FILE_ARMCONSOLELOG
					: BroadBandCommandConstants.FILE_CONSOLELOG;
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_START_PARODUS, consoleLog,
					BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (CommonMethods.isNotNull(response)) {
				// get the time stamp from response output
				response = CommonMethods.patternFinder(response,
						BroadBandTestConstants.PATTERN_TIME_FORMAT_IN_LOG_FILE);
				if (CommonMethods.isNotNull(response)) {
					// convert the time stamp to formatted output
					dateTimeConsole = format.parse(response);
					status = true;
				}
			}
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL: Successfully verified log message in PARODUS log file and time stamp from the response is: "
								+ dateTimeConsole);
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION: Get PARODUS boot time value");
			LOGGER.info("STEP 6: ACTION: Boot time = timestamp for connect to SSL - startpardous");
			LOGGER.info("STEP 6: EXPECTED: Should the get the Boot time for Parodus");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the PARODUS boot time value";
			// calculate the time difference to get the parodus boot time
			timeDifference = dateTimeParodus.getTime() - dateTimeConsole.getTime();
			status = timeDifference != BroadBandTestConstants.CONSTANT_0;
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully verified PARODUS boot time value in min: "
						+ timeDifference / BroadBandTestConstants.CONSTANT_1000);
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Verify Webpa is working fine after reconnection");
			LOGGER.info("STEP 7: ACTION: Execute webpa get command for Device.DeviceInfo.SerialNumber");
			LOGGER.info("STEP 7: EXPECTED: Webpa should success and should get the response ");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.SerialNumber";
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL: Successfully received serial number from webpa after reconnection");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s8";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Verify parodus connection is IPv4/IPv6 mode");
			LOGGER.info("STEP 8: ACTION: Execute command: netstat -an | grep 8080");
			LOGGER.info("STEP 8: EXPECTED: Response should contain either IPv4/IPv6");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get response from netstat command";
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_NETSTAT_8080);
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "Parodus connection is not established with either IPv4 or IPv6 mode";
				// get the IP address from the response
				response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_IP_NESTAT);
				status = CommonMethods.isNotNull(response);
				if (status && CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.DELIMITER_COLON)) {
					LOGGER.info("Parodus connection established with IPv6 mode");
				} else if (CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.DOT_OPERATOR)) {
					LOGGER.info("Parodus connection established with IPv4 mode");
				}
			}
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Successfully verified parodus connection mode");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying parodus nopoll" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-NOPOLL-1001");
	}

	/**
	 *
	 * Test Case # 2: Verify Webpa commands when Parodus is Enabled
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>STEP 1: verify webpa Get-Attributes request is working</li>
	 * <li>STEP 2: verify webpa Set-Attributes request is working</li>
	 * <li>STEP 3: verify TEST and SET requests are working</li>
	 * <li>STEP 4: verify POST request is working</li>
	 * <li>STEP 5: verify PUT request is working</li>
	 * <li>STEP 6: verify DELETE is working</li>
	 * </ol>
	 *
	 * @author Sumathi Gunasekaran
	 * @refactor Govardhan
	 * 
	 * @param device {@link Dut}
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.SYSTEM })
	@TestDetails(testUID = "TC-RDKB-PARODUS-ENBL-1004")
	public void testVerifyWebPAEnableParodus(Dut device) throws IOException, JSONException {

		// variable to store errorMessage
		String errorMessage = null;
		// variable to store testcaseID
		String testCaseId = "TC-RDKB-PARODUS-ENBL-004";
		// variable to store teststepNumber
		String testStepNumber = "s1";
		// variable to store status
		boolean status = false;
		// Variable to store response
		String response = null;
		// string to store the webpaserver response
		WebPaServerResponse webPaServerResponse = null;
		// String to store the added table row number
		String tableRowNumber = null;

		try {
			LOGGER.info("************* STARTING PRE-CONFIGURATIONS *************");
			LOGGER.info("***********************************************************");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-ENBL-1004");
			LOGGER.info("TEST DESCRIPTION: Verify Webpa Commands When Parodus is Enabled ");
			LOGGER.info("***********************************************************");

			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 1:DESCRIPTION: Verify webpa Get-Attributes request is working");
			LOGGER.info("STEP 1: ACTION : Execute webpa get command for webpa param-"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL);
			LOGGER.info("STEP 1:EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			status = BroadBandWebPaUtils.executeWebPaCommandGetAttribute(tapEnv, device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL, BroadBandTestConstants.NOTIFY);
			errorMessage = "Failed to get the webpa request for parameter:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully verified get request through webpa command for parameter :"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CM_IP_PARENTAL_CONTROL);
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s2";
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 2:DESCRIPTION: Verify webpa Set-Attributes request is working");
			LOGGER.info("STEP 2: ACTION : Execute webpa set command"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			LOGGER.info("STEP 2:EXPECTED : The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");

			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.STRING_TEST_1, BroadBandTestConstants.CONSTANT_0);
			errorMessage = CommonUtils.concatStringUsingStringBuffer(
					"Failed to Set Attribute value for WebPa parameter:",
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully Set Attribute for webpa parameter: :"
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s3";
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 3:DESCRIPTION: Verify TEST and SET requests are working");
			LOGGER.info("STEP 3: ACTION : Execute webpa set parameter and test the value"
					+ BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);
			LOGGER.info("STEP 3:EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			errorMessage = "Failed to Set and Test value for WebPa parameter:";

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully Verified Test and Test Attribute:");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s4";
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: verify POST request is working");
			LOGGER.info("STEP 4: ACTION : Execute webpa post command"
					+ BroadBandWebPaConstants.WEBPA_PARAM_PARENTAL_CONTROL_MANAGED_DEVICES);
			LOGGER.info("STEP 4: EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");

			Map<String, List<String>> macFilterTable = new HashMap<String, List<String>>();
			List<String> MacAddressList = new ArrayList<String>();
			MacAddressList.add(BroadBandTestConstants.DUMMY_MAC);
			List<String> Type = new ArrayList<String>();
			Type.add(BroadBandTestConstants.BLOCK);
			List<String> Description = new ArrayList<String>();
			Description.add(BroadBandTestConstants.STRING_TEST_1);
			// adding to the Map.
			macFilterTable.put(BroadBandTestConstants.MACADDRESS, MacAddressList);
			macFilterTable.put(BroadBandTestConstants.TYPE, Type);
			macFilterTable.put(BroadBandTestConstants.DESCRIPTION, Description);
			webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_PARAM_PARENTAL_CONTROL_MANAGED_DEVICES, macFilterTable);
			// Saving the Table Row number to do delete operation in further
			// steps.
			tableRowNumber = webPaServerResponse.getRow();
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter filter by Webpa POST command";
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL: Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s5";
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: verify PUT request is working");
			LOGGER.info("STEP 5: ACTION: Execute webpa command for PUT request"
					+ BroadBandWebPaConstants.WEBPA_PARAM_SERVER_POOL_STATIC_ADDRESS);
			LOGGER.info("STEP 5: EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			HashMap<String, HashMap<String, List<String>>> map = new HashMap<String, HashMap<String, List<String>>>();
			Map<String, List<String>> macFilterTable1 = new HashMap<String, List<String>>();
			List<String> Yiaddr = new ArrayList<>();
			Yiaddr.add(BroadBandConnectedClientUtils.getReservedIpBetweenDhcpRangeFromRouter(tapEnv, device,
					BroadBandTestConstants.STRING_NULL_IP));
			macFilterTable1.put(BroadBandTestConstants.IPADDRESS, Yiaddr);
			map.put(RDKBTestConstants.STRING_ZERO, (HashMap<String, List<String>>) macFilterTable1);
			LOGGER.info("Printing Map Value:" + map);
			webPaServerResponse = tapEnv.putWebpaTableParamUsingRestApi(device,
					BroadBandWebPaConstants.WEBPA_PARAM_SERVER_POOL_STATIC_ADDRESS, map);
			status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			errorMessage = "Unable to add the wifi Mac Address to the  MAC Filter filter by Webpa POST command";
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL: Successfully added the wifi Mac Address to the  MAC Filter filter by Webpa POST command");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

			testStepNumber = "s6";
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("***********************************************************");
			LOGGER.info("STEP 6: DESCRIPTION: verify DELETE is working");
			LOGGER.info("STEP 6: ACTION:Execute webpa command for Delete request on the table row");
			LOGGER.info("STEP 6: EXPECTED: The webpa command should Execute successfully");
			LOGGER.info("***********************************************************");
			if (CommonMethods.isNotNull(tableRowNumber)) {
				webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRowNumber);
				LOGGER.info(webPaServerResponse.toString());
				status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
			}
			errorMessage = "Unable to delete row using rest Api for the given table row number";
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully deleted the row from the table");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error(
					"Exception Occurred while Verifying webpa commands to check parodus functionality " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
					errorMessage, true);
		}
	}

	/**
	 * Verify parodus reconnect with jitter algorithm
	 * <ol>
	 * <li>Copy and update /nvram/webpa_cfg.json to simulate parodus reconnect</li>
	 * <li>Clear PARODUSlog and restart parodus process</li>
	 * <li>Verify parodus restarted with updated server ip</li>
	 * <li>Verify backoffRetryTime values within min-max ranges</li>
	 * <li>Replace webpa_cfg.json file and restart parodus process</li>
	 * <li>Verify parodus restarted and reconnected successfully</li>
	 * </ol>
	 * 
	 * @author Ashwin sankara
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-PARODUS-1010")
	public void testVerifyParodusReconnectJitter(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PARODUS-010";
		String stepNum = "s1";
		String errorMessage = null;
		String response = null;
		boolean status = false;
		BroadBandResultObject result = new BroadBandResultObject();
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1010");
		LOGGER.info("TEST DESCRIPTION: Verify parodus reconnect with jitter algorithm");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Copy and update /nvram/webpa_cfg.json to simulate parodus reconnect");
		LOGGER.info("2. Clear PARODUSlog and restart parodus process");
		LOGGER.info("3. Verify parodus restarted with updated server ip");
		LOGGER.info("4. Verify backoffRetryTime values within min-max ranges");
		LOGGER.info("5. Replace webpa_cfg.json file and restart parodus process");
		LOGGER.info("6. Verify parodus restarted and reconnected successfully");

		LOGGER.info("#######################################################################################");

		try {
			// Getting INCORRECT_WEBPA_URL from props
			String INCORRECT_WEBPA_URL = BroadbandPropertyFileHandler.getIncorrectWebparURL();

			stepNum = "s1";
			errorMessage = "Failed to download wbpa_cfg.json from autovault service";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Copy and update /nvram/webpa_cfg.json to simulate parodus reconnect");
			LOGGER.info(
					"STEP 1: ACTION : 1. Download file from autovault to /tmp/wbpa_cfg.json\n2. Execute commands:\ncp /nvram/webpa_cfg.json /nvram/webpa_cfg.json_bkp\ncp /tmp/wbpacfg.json /nvram/webpa_cfg.json\ngrep "
							+ BroadbandPropertyFileHandler.getAutoVaultDownloadURL() + " /nvram/webpa_cfg.json");
			LOGGER.info("STEP 1: EXPECTED : Updated parameters in webpa_cfg.json to simulate parodus reconnect");
			LOGGER.info("**********************************************************************************");

			if (CommonUtils.downloadFileUsingAutoVault(device, tapEnv,
					BroadBandCommandConstants.FILE_PATH_AUTOVAULT_WBPA_CFG_JSON,
					BroadBandCommandConstants.DIRECTORY_TMP)) {
				errorMessage = "Failed to find incorrect webpa url in /nvram/webpa_cfg.json after copying";
				tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_COPY,
								BroadBandCommandConstants.FILE_WEBPA_CFG, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
								BroadBandCommandConstants.WEBPA_CFG_JSON_BKP_FILE));
				tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_COPY, BroadBandCommandConstants.FILE_PATH_TMP_WBPA_CFG_JSON,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_WEBPA_CFG));
				status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						INCORRECT_WEBPA_URL, BroadBandCommandConstants.FILE_WEBPA_CFG));
			}

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Updated parameters in webpa_cfg.json to simulate parodus reconnect");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Failed to clear PARODUSlog";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Clear PARODUSlog and restart parodus process");
			LOGGER.info(
					"STEP 2: ACTION : Execute commands:\n1. echo > /rdklogs/logs/PARODUSlog.txt.0\n2. killall parodus3. pidof parodus");
			LOGGER.info("STEP 2: EXPECTED : Parodus process is killed after clearing log file");
			LOGGER.info("**********************************************************************************");

			if (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_PARODUS)) {
				errorMessage = "Failed to restart parodus process";
				status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
						BroadBandTestConstants.PARODUS_PROCESS_NAME);
			}

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Parodus process is killed after clearing log file");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Unable to find updated webpa url in PARODUSlog";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify parodus restarted with updated server ip");
			LOGGER.info("STEP 3: ACTION : Execute command:grep \"+" + INCORRECT_WEBPA_URL
					+ "+\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 3: EXPECTED : Parodus process restarted with updated server url");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device, INCORRECT_WEBPA_URL,
					BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Parodus process restarted with updated server url");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Failed to get backoffRetryTime log messages in PARODUSlog";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify backoffRetryTime values within min-max ranges");
			LOGGER.info(
					"STEP 4: ACTION : 1. Execute command:grep backoffRetryTime /rdklogs/logs/PARODUSlog.txt.0 2. Check backoffRetryTime values between 3 and max values logged respectively");
			LOGGER.info("STEP 4: EXPECTED : Values of backoffRetryTime are within min-max ranges");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_BACK_OFF_RETRY_TIME,
					BroadBandCommandConstants.LOG_FILE_PARODUS);
			if (CommonMethods.isNotNull(response)) {
				result = ParodusUtils.verifyParodusReconnectJitter(response);
				errorMessage = result.getErrorMessage();
				status = result.isStatus();
			}

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Values of backoffRetryTime are within min-max ranges");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Failed to clear PARODUSlog";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Replace webpa_cfg.json file and restart parodus process");
			LOGGER.info(
					"STEP 5: ACTION : Execute commands:1. cp /nvram/webpa_cfg.json_bkp /nvram/webpa_cfg.json 2. echo > /rdklogs/logs/PARODUSlog.txt.0 3. killall parodus 4. pidof parodus");
			LOGGER.info("STEP 5: EXPECTED : Parodus process is restarted after replacing webpa_cfg.json file");
			LOGGER.info("**********************************************************************************");

			if (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.LOG_FILE_PARODUS)) {
				errorMessage = "Failed to restart parodus process";
				CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device, BroadBandCommandConstants.FILE_WEBPA_CFG);
				tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
						BroadBandCommandConstants.CMD_COPY, BroadBandCommandConstants.WEBPA_CFG_JSON_BKP_FILE,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_WEBPA_CFG));
				status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
						BroadBandTestConstants.PARODUS_PROCESS_NAME);
			}

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Parodus process is restarted after replacing webpa_cfg.json file");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Unable to find parodus connected log message after restart";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify parodus restarted and reconnected successfully");
			LOGGER.info(
					"STEP 6: ACTION : Execute command:grep \"Connected to server over SSL\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info("STEP 6: EXPECTED : Parodus reconnected successfully after restart");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_CONNECTED_TO_SERVER_OVER_SSL,
					BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Parodus reconnected successfully after restart");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info(
					"POST-CONDITION : DESCRIPTION : Remove /nvram/webpa_cfg.json_bkp and verify webpa is up on the device");
			LOGGER.info(
					"POST-CONDITION : ACTION : 1. Execute command: rm -rf /nvram/webpa_cfg.json_bkp\n2. Poll and verify webpa command to get device serial number");
			LOGGER.info("POST-CONDITION : EXPECTED : Post condition executed successfully");

			status = false;
			errorMessage = "Failed to remove file: /nvram/webpa_cfg.json_bkp";
			if (CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
					BroadBandCommandConstants.WEBPA_CFG_JSON_BKP_FILE)) {
				errorMessage = "Failed to verify webpa working on the device";
				status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			}

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : " + errorMessage);
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-1010");
	}

	/**
	 * Testcase to verify parodus drop process privileges
	 * <ol>
	 * <li>Verify nonroot support feature parameter is enabled by deafult</li>
	 * <li>Check parodus process run as non-root</li>
	 * <li>Check webpa process is running as root</li>
	 * <li>Kill parodus process in the device</li>
	 * <li>Verify minidump is created for killed parodus process</li>
	 * <li>Verify parodus is recreated with non-root after selfheal</li>
	 * <li>Verify webpa set is working</li>
	 * <li>Verify webpa get is working</li>
	 * <li>Disable nonroot support feature using RFC</li>
	 * <li>Verify nonroot support feature parameter is disabled using RFC</li>
	 * <li>Check parodus process run as root</li>
	 * <li>Check webpa process is running as root</li>
	 * <li>Kill parodus process in the device</li>
	 * <li>Verify minidump is created for killed parodus process</li>
	 * <li>Verify parodus process is recreated after selfheal</li>
	 * <li>Verify webpa set is working</li>
	 * <li>Verify webpa get is working</li>
	 * <li>Enable nonroot support feature using RFC</li>
	 * <li>Verify nonroot support feature parameter is enabled using RFC</li>
	 * <li>Check parodus process run as non-root</li>
	 * <li>Check webpa process is running as root</li>
	 * <li>Verify parodus is running as unprivilege mode in CapDebug.txt</li>
	 * <li>Verify unprivilege user name is non-root in CapDebug.txt</li>
	 * </ol>
	 * 
	 * @author Betel Costrow
	 * @refactor Govardhan
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-PARODUS-1011")
	public void testTovVerifyNonRootSupport(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PARODUS-011";
		String stepNum = "s1";
		String errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Enable not enabled by default";
		boolean status = false;
		String response = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1011");
		LOGGER.info("TEST DESCRIPTION: Testcase to verify parodus drop process privileges");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify nonroot support feature parameter is enabled by deafult ");
		LOGGER.info("2. Check parodus process run as non-root");
		LOGGER.info("3. Check webpa process is running as root");
		LOGGER.info("4. Kill parodus process in the device");
		LOGGER.info("5. Verify minidump is created for killed parodus process ");
		LOGGER.info("6. Verify parodus is recreated with non-root after selfheal");
		LOGGER.info("7. Verify webpa set is working ");
		LOGGER.info("8. Verify webpa get is working ");
		LOGGER.info("9. Disable nonroot support feature using RFC");
		LOGGER.info("10. Verify nonroot support feature parameter is disabled using RFC ");
		LOGGER.info("11. Check parodus process run as root");
		LOGGER.info("12. Check webpa process is running as root");
		LOGGER.info("13. Kill parodus process in the device");
		LOGGER.info("14. Verify minidump is created for killed parodus process ");
		LOGGER.info("15. Verify parodus process is recreated after selfheal");
		LOGGER.info("16. Verify webpa set is working ");
		LOGGER.info("17. Verify webpa get is working ");
		LOGGER.info("18. Enable nonroot support feature using RFC");
		LOGGER.info("19. Verify nonroot support feature parameter is enabled using RFC ");
		LOGGER.info("20. Check parodus process run as non-root");
		LOGGER.info("21. Check webpa process is running as root");
		LOGGER.info("22. Verify parodus is running as unprivilege mode in CapDebug.txt  ");
		LOGGER.info("23. Verify unprivilege user name is non-root in CapDebug.txt  ");

		LOGGER.info("#######################################################################################");

		try {

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify nonroot support feature parameter is enabled by deafult ");
			LOGGER.info(
					"STEP 1: ACTION : Execute:Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Enable");
			LOGGER.info("STEP 1: EXPECTED : Nonroot support feature should be enabled ");
			LOGGER.info("**********************************************************************************");

			/*
			 * status =
			 * BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device,
			 * tapEnv, BroadBandWebPaConstants.TR181_PARAM_NONROOT_SUPPORT,
			 * BroadBandTestConstants.TRUE);
			 */

			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.TR181_PARAM_NONROOT_SUPPORT_BLOCKLIST,
					BroadBandTestConstants.CMD_NO_BLOCKLIST_PROCESS);

			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Sucessfully verified non-root support feature is enabled by default");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Parodus process not running as non-root";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Check parodus process run as non-root");
			LOGGER.info("STEP 2: ACTION : Execute:ps | grep -nri -E \"parodus\" | grep bin");
			LOGGER.info("STEP 2: EXPECTED : Response should have non-root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
					.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_PARODUS));
			if (CommonMethods.isNotNull(response)) {
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.STRING_NON_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Successfuly verified parodus process is running as non-root");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "webpa process not running as root ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Check webpa process is running as root");
			LOGGER.info("STEP 3: ACTION : Execute:ps | grep -nri -E \"webpa\" | grep bin");
			LOGGER.info("STEP 3: EXPECTED : Response should have root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_WEBPA));
			} else {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_WEBPA));
			}
			if (CommonMethods.isNotNull(response)) {
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.STRING_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Successfuly verified webpa process is running as root");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Parodus process is not killed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Kill parodus process in the device");
			LOGGER.info("STEP 4: ACTION : Execute:1.get parodus process using pidof parodus2.kill -1 <pidof parodus>");
			LOGGER.info("STEP 4: EXPECTED : Parodus process should be killed");
			LOGGER.info("**********************************************************************************");

			CommonUtils.clearLogFile(tapEnv, device, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
			status = BroadBandCommonUtils.killAndCheckProcess(device, tapEnv,
					BroadBandTestConstants.PROCESS_NAME_PARODUS);

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Successfully verified parodus process id recreated by selfheal after killed");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "minidump is not created after parodus process killed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify minidump is created for killed parodus process ");
			LOGGER.info("STEP 5: ACTION : Execute:grep -I \"Response code: 200\" /rdklogs/logs/core_log.txt");
			LOGGER.info("STEP 5: EXPECTED : Response should have 200 success message ");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_RESPONSE_200, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Verified minidump is created for killed parodus process");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Parodus process is not recreated after selfheal";
			status = false;
			long startTimeStamp = System.currentTimeMillis();

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify parodus is recreated with non-root after selfheal");
			LOGGER.info(
					"STEP 6: ACTION : Execute:ps | grep -nri -E \"parodus\" | grep binwait for 7 to 20min to get new process id");
			LOGGER.info("STEP 6: EXPECTED : Response should have non-root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			do {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_PARODUS));
				status = CommonMethods.isNotNull(response) && CommonUtils
						.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_NON_ROOT);
			} while (!status
					&& (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Successfuly verified parodus process is running as non-root");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			String WEBPA_URL = BroadbandPropertyFileHandler.getWebpaServerURL();

			stepNum = "s7";
			errorMessage = "Webpa set process is not working";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify webpa set is working ");
			LOGGER.info("STEP 7: ACTION : Execute:curl -H <SAT_TOKEN> -X PATCH " + WEBPA_URL
					+ "<ECM_MAC_ADDRESS> /config -d \"{\"parameters\":[{\"dataType\":1,\"name\":\"Device.WiFi.SSID.10001.SSID\",\"value\":\"2_4Wifi\"}]}\"");
			LOGGER.info("STEP 7: EXPECTED : Should get 200 success msg for webpa set process");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_2GHZ_BAND,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Successfully verified webpa set is working");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Webpa get process is not working";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify webpa get is working ");
			LOGGER.info("STEP 8: ACTION : Execute:curl -X GET " + WEBPA_URL
					+ ":<ecm_mac>/config?names=Device.WiFi.SSID.10001.SSID -H \"AuThorization:Bearer <sat token>\"");
			LOGGER.info("STEP 8: EXPECTED : Should get 200 success msg for webpa get process");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.PRIVATEWIFI_NAME_FOR_2GHZ_BAND);

			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Successfully verified webpa get is working");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Parodus process not running as root";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Check parodus process run as root");
			LOGGER.info("STEP 9: ACTION : Execute:ps | grep -nri -E \"parodus\" | grep bin");
			LOGGER.info("STEP 9: EXPECTED : Response should have root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
					.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_PARODUS));
			if (CommonMethods.isNotNull(response)) {
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.STRING_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Successfuly verified parodus process is running as root");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "webpa process not running as root ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Check webpa process is running as root");
			LOGGER.info("STEP 10: ACTION : Execute:ps | grep -nri -E \"webpa\" | grep bin");
			LOGGER.info("STEP 10: EXPECTED : Response should have root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_WEBPA));
			} else {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_WEBPA));
			}
			if (CommonMethods.isNotNull(response)) {
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.STRING_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 10: ACTUAL : Successfuly verified webpa process is running as root");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Parodus process is not killed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Kill parodus process in the device");
			LOGGER.info("STEP 11: ACTION : Execute:1.get parodus process using pidof parodus2.kill -1 <pidof parodus>");
			LOGGER.info("STEP 11: EXPECTED : Parodus process should be killed");
			LOGGER.info("**********************************************************************************");

			CommonUtils.clearLogFile(tapEnv, device, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB);
			status = BroadBandCommonUtils.killAndCheckProcess(device, tapEnv,
					BroadBandTestConstants.PROCESS_NAME_PARODUS);

			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : Successfully verified parodus process id recreated by selfheal after killed");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s12";
			errorMessage = "minidump is not created after parodus process killed";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 12: DESCRIPTION : Verify minidump is created for killed parodus process ");
			LOGGER.info("STEP 12: ACTION : Execute:grep -I \"Response code: 200\" /rdklogs/logs/core_log.txt");
			LOGGER.info("STEP 12: EXPECTED : Response should have 200 success message ");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_RESPONSE_200, CrashConstants.LOG_FILE_FOR_CRASHES_RDKB,
					BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 12: ACTUAL : Verified minidump is created for killed parodus process");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s13";
			errorMessage = "Parodus process is not recreated";
			status = false;
			startTimeStamp = System.currentTimeMillis();

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Verify parodus process is recreated after selfheal");
			LOGGER.info(
					"STEP 13: ACTION : Execute:ps | grep -nri -E \"parodus\" | grep binwait for 7 to 20min to get new process id");
			LOGGER.info("STEP 13: EXPECTED : Response should have root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			do {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_PARODUS));
				status = CommonMethods.isNotNull(response) && CommonUtils
						.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_ROOT);
			} while (!status
					&& (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Successfuly verified parodus process is running as root");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s14";
			errorMessage = "Webpa set process is not working";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Verify webpa set is working ");
			LOGGER.info("STEP 14: ACTION : Execute:curl -H <SAT_TOKEN> -X PATCH " + WEBPA_URL
					+ ":<ECM_MAC_ADDRESS> /config -d \"{\"parameters\":[{\"dataType\":1,\"name\":\"Device.WiFi.SSID.10001.SSID\",\"value\":\"2_4Wifi\"}]}\"");
			LOGGER.info("STEP 14: EXPECTED : Should get 200 success msg for webpa set process");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.SSID_FOR_2GHZ_PRIVATE_WIFI,
					BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

			if (status) {
				LOGGER.info("STEP 14: ACTUAL : Successfully verified webpa set is working");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s15";
			errorMessage = "Webpa get process is not working";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION : Verify webpa get is working ");
			LOGGER.info("STEP 15: ACTION : Execute:curl -X GET " + WEBPA_URL
					+ ":<ecm_mac>/config?names=Device.WiFi.SSID.10001.SSID -H \"AuThorization:Bearer <sat token>\"");
			LOGGER.info("STEP 15: EXPECTED : Should get 200 success msg for webpa get process");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
					BroadBandTestConstants.SSID_FOR_2GHZ_PRIVATE_WIFI);

			if (status) {
				LOGGER.info("STEP 15: ACTUAL : Successfully verified webpa get is working");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s16";
			errorMessage = "Parodus process not running as non-root";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION : Check parodus process run as non-root");
			LOGGER.info("STEP 16: ACTION : Execute:ps | grep -nri -E \"parodus\" | grep bin");
			LOGGER.info("STEP 16: EXPECTED : Response should have non-root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			/*
			 * Boolean isSingleRebootDevice = null;
			 * 
			 * isSingleRebootDevice =
			 * BroadbandPropertyFileHandler.isSingleRebootRFCFeatureDevice(device);
			 * 
			 * if (isSingleRebootDevice) { status =
			 * BroadBandRfcFeatureControlUtils.rfcFeatureWithSingleRebootRetrievewNow(
			 * tapEnv, device, BroadBandTestConstants.CONFIGURABLE_NONROOT_SUPPORT, true); }
			 * else { status =
			 * BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
			 * BroadBandTestConstants.CONFIGURABLE_NONROOT_SUPPORT, true); }
			 */

			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
					.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_PARODUS));
			if (CommonMethods.isNotNull(response)) {
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.STRING_NON_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 16: ACTUAL : Successfuly verified parodus process is running as non-root");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s17";
			errorMessage = "webpa process not running as root ";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 17: DESCRIPTION : Check webpa process is running as root");
			LOGGER.info("STEP 17: ACTION : Execute:ps | grep -nri -E \"webpa\" | grep bin");
			LOGGER.info("STEP 17: EXPECTED : Response should have root along with parodus process id");
			LOGGER.info("**********************************************************************************");

			/*
			 * status =
			 * BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device,
			 * tapEnv, BroadBandWebPaConstants.TR181_PARAM_NONROOT_SUPPORT,
			 * BroadBandTestConstants.TRUE);
			 */

			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_WEBPA));
			} else {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
						.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_WEBPA));
			}
			if (CommonMethods.isNotNull(response)) {
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTestConstants.STRING_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 17: ACTUAL : Sucessfully verified non-root support feature is enabled by RFC");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			/*
			 * stepNum = "s20"; errorMessage = "Parodus process not running as non-root";
			 * status = false;
			 * 
			 * LOGGER.info(
			 * "**********************************************************************************"
			 * );
			 * LOGGER.info("STEP 20: DESCRIPTION : Check parodus process run as non-root");
			 * LOGGER.
			 * info("STEP 20: ACTION : Execute:ps | grep -nri -E \"parodus\" | grep bin");
			 * LOGGER.
			 * info("STEP 20: EXPECTED : Response should have non-root along with parodus process id"
			 * ); LOGGER.info(
			 * "**********************************************************************************"
			 * );
			 * 
			 * response = tapEnv.executeCommandUsingSsh(device,
			 * BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
			 * .replace(BroadBandTestConstants.STRING_REPLACE,
			 * BroadBandTestConstants.PROCESS_NAME_PARODUS)); if
			 * (CommonMethods.isNotNull(response)) { status =
			 * CommonUtils.isGivenStringAvailableInCommandOutput(response,
			 * BroadBandTestConstants.STRING_NON_ROOT); }
			 * 
			 * if (status) { LOGGER.
			 * info("STEP 20: ACTUAL : Successfuly verified parodus process is running as non-root"
			 * ); } else { LOGGER.error("STEP 20: ACTUAL : " + errorMessage); }
			 * 
			 * tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
			 * errorMessage, false);
			 * 
			 * LOGGER.info(
			 * "**********************************************************************************"
			 * );
			 * 
			 * stepNum = "s21"; errorMessage = "webpa process not running as root "; status
			 * = false;
			 * 
			 * LOGGER.info(
			 * "**********************************************************************************"
			 * );
			 * LOGGER.info("STEP 21: DESCRIPTION : Check webpa process is running as root");
			 * LOGGER.
			 * info("STEP 21: ACTION : Execute:ps | grep -nri -E \"webpa\" | grep bin");
			 * LOGGER.
			 * info("STEP 21: EXPECTED : Response should have root along with parodus process id"
			 * ); LOGGER.info(
			 * "**********************************************************************************"
			 * );
			 * 
			 * if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) { response =
			 * tapEnv.executeCommandOnAtom(device,
			 * BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
			 * .replace(BroadBandTestConstants.STRING_REPLACE,
			 * BroadBandTestConstants.PROCESS_NAME_WEBPA)); } else { response =
			 * tapEnv.executeCommandUsingSsh(device,
			 * BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
			 * .replace(BroadBandTestConstants.STRING_REPLACE,
			 * BroadBandTestConstants.PROCESS_NAME_WEBPA)); } if
			 * (CommonMethods.isNotNull(response)) { status =
			 * CommonUtils.isGivenStringAvailableInCommandOutput(response,
			 * BroadBandTestConstants.STRING_ROOT); }
			 * 
			 * if (status) { LOGGER.
			 * info("STEP 21: ACTUAL : Successfuly verified webpa process is running as root"
			 * ); } else { LOGGER.error("STEP 21: ACTUAL : " + errorMessage); }
			 * 
			 * tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status,
			 * errorMessage, false);
			 * 
			 * LOGGER.info(
			 * "**********************************************************************************"
			 * );
			 */

			stepNum = "s18";
			errorMessage = "/rdklogs/logs/CapDebug.txt is empty";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 18: DESCRIPTION : Verify parodus is running as unprivilege mode in CapDebug.txt  ");
			LOGGER.info(
					"STEP 18: ACTION : Execute:grep -I \"Dropping root privilege of parodus: runs as unprivilege mode\"  /rdklogs/logs/CapDebug.txt");
			LOGGER.info("STEP 18: EXPECTED : Response should have uprivilege mode");
			LOGGER.info("**********************************************************************************");

			response = CommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.LOG_MESSAGE_UNPRIVILEGE,
					BroadBandCommandConstants.CMD_TO_GET_CAPDEBUG, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (CommonMethods.isNotNull(response)) {
				errorMessage = "parodus is not running in unprivilege mode";
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTraceConstants.LOG_MESSAGE_UNPRIVILEGE_MODE);
			}

			if (status) {
				LOGGER.info("STEP 18: ACTUAL : Successfully verified parodus running as unprivilage mode");
			} else {
				LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s19";
			errorMessage = "/rdklogs/logs/CapDebug.txt is empty";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 19: DESCRIPTION : Verify unprivilege user name is non-root in CapDebug.txt  ");
			LOGGER.info(
					"STEP 19: ACTION : Execute:grep -I \"unprivilege user name: non-root\"  /rdklogs/logs/CapDebug.txt");
			LOGGER.info("STEP 19: EXPECTED : Response should have non-root for user name");
			LOGGER.info("**********************************************************************************");

			if (CommonMethods.isNotNull(response)) {
				errorMessage = "unprivilege user name is not having non-root process";
				status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
						BroadBandTraceConstants.LOG_MESSAGE_UNPRIVILEGE_MODE_NON_ROOT);
			}

			if (status) {
				LOGGER.info("STEP 19: ACTUAL : Successfully verified unprivilege mode running as non-root");
			} else {
				LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			status = false;

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : delete RFC feature");
			LOGGER.info(
					"POST-CONDITION : ACTION : 1)Send following HTTP DELETE request using POSTMAN/rest client:: https://<url>/featureControl/clear?estbMacAddress=<mac>&featureName=nonroot_support2) reboot the device twice");
			LOGGER.info("POST-CONDITION : EXPECTED : RFC feature should be removed successfully");

			status = (HttpStatus.SC_OK == BroadBandRfcFeatureControlUtils.clearSettingsInProxyXconfDcmServerForRDKB(
					device, tapEnv, false, BroadBandTestConstants.CONFIGURABLE_NONROOT_SUPPORT));

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-1011");
	}

	/**
	 * Verify themis token fetch by passing Partner-ID as a request header
	 * <ol>
	 * <li>Reboot the device</li>
	 * <li>Enable webconfig once device is up after reboot</li>
	 * <li>Verify parodus is running successfully</li>
	 * <li>Verify right partner ID is included from parodus log and webconfig
	 * log</li>
	 * <li>Verify issuer curl response http_code 200 in Parodus & Webconfig log</li>
	 * <li>Verify curl response path in Webconfig log</li>
	 * <li>Verify when partner ID is empty in Curl command token fetch is
	 * success</li>
	 * <li>Kill parodus service running in device</li>
	 * <li>Restart parodus with some invalid token server url</li>
	 * <li>Verify "Failed to create new token" is present in parodus log</li>
	 * <li>Verify parodus is running eventhough token fetch fails</li>
	 * 
	 * @author Geetha DS
	 * @refactor Govardhan
	 *           </ol>
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-PARODUS-1012")

	public void testToVerifyThemisTokenFetch(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PARODUS-012";
		String stepNum = "s1";
		String errorMessage = null;
		String response = null;
		String response1 = null;
		String partnerId = null;
		boolean status = false;
		String parodusCmd = null;
		String partnerIdParodus = null;
		String partnerIdWebconfig = null;
		String deviceMacFromConfig = null;
		String command = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1012");
		LOGGER.info("TEST DESCRIPTION: Verify themis token fetch by passing Partner-ID as a request header");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Reboot the device");
		LOGGER.info("2. Enable webconfig once device is up after reboot");
		LOGGER.info("3. Verify parodus is running successfully");
		LOGGER.info("4. Verify right partner ID is included from parodus log and webconfig log");
		LOGGER.info("5. Verify issuer curl  response http_code 200 in Parodus & Webconfig log");
		LOGGER.info("6. Verify curl response path in Webconfig log");
		LOGGER.info("7. Verify when partner ID is empty in Curl command token fetch is success");
		LOGGER.info("8. Kill parodus service running in device");
		LOGGER.info("9. Restart parodus with some invalid token server url");
		LOGGER.info("10. Verify \"Failed to create new token\" is present in parodus log");
		LOGGER.info("11. Verify parodus is running eventhough token fetch fails");
		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Unable to reboot device";
			status = false;
			LOGGER.info("*********************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Reboot the device");
			LOGGER.info("STEP 1: ACTION : Execute Command:reboot");
			LOGGER.info("STEP 1: EXPECTED : Device must go on reboot");
			LOGGER.info("**********************************************");

			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : Device went on reboot successfully");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			LOGGER.info("**********************************************************************************");

			stepNum = "s2";
			errorMessage = "Unable to enable webconfig";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Enable webconfig once device is up after reboot");
			LOGGER.info("STEP 2: ACTION : Execute Command:tr181.Device.X_RDK_WebConfig.RfcEnable bool true");
			LOGGER.info("STEP 2: EXPECTED : Webconfig must be enabled");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WEBCONFIG_RFC_ENABLE, BroadBandTestConstants.TRUE);
			if (!status) {
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WEBCONFIG_RFC_ENABLE, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : WebConfig feature is enabled");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s3";
			errorMessage = "Parodus process is not up and running in the device";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify parodus is running successfully");
			LOGGER.info("STEP 3: ACTION : Execute Command: pidof parodus");
			LOGGER.info("STEP 3: EXPECTED : Parodus must be running successfully");
			LOGGER.info("**********************************************************************************");
			if (CommonMethods
					.isNotNull(CommonUtils.getPidOfProcess(device, tapEnv, StbProcess.PARODUS.getProcessName()))) {
				response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PARODUS_EXECUTION_PATH);
				if (CommonMethods.isNotNull(response)) {
					parodusCmd = CommonMethods.patternFinder(response,
							BroadBandTestConstants.PATTERN_PARODUS_EXECUTION_PATH);
					status = CommonMethods.isNotNull(parodusCmd);
				}
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Parodus process is up and running");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s4";
			errorMessage = "Partner ID present in parodus log is not as expected";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify right partner ID is included from parodus log and webconfig log");
			LOGGER.info(
					"STEP 4: ACTION : Execute command: grep -i \"X-Midt-Partner-Id\" /rdklogs/logs/PARODUSlog.txt.0; grep -i \"X-Midt-Partner-Id\" /rdklogs/logs/WEBCONFIGlog.txt.0; Obtain value of Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PartnerId parameter and verify same value is logged");
			LOGGER.info(
					"STEP 4: EXPECTED : Partner ID taken in Parodus log & Webconfig log must be same as WebPA parameter value");
			LOGGER.info("**********************************************************************************");

			// Get the partner id using webpa get operation
			partnerId = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
			if (CommonMethods.isNotNull(partnerId)) {
				// Search the partner id log message in webconfig log
				response1 = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
						BroadBandTraceConstants.LOG_MESSAGE_XMIDT_PARTNER_ID,
						BroadBandCommandConstants.FILE_RDKLOGS_WEBCONFIG_LOG,
						BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
				// Search the partner id log message in parodus log
				response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
						BroadBandTraceConstants.LOG_MESSAGE_XMIDT_PARTNER_ID,
						BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (CommonMethods.isNotNull(response) && CommonMethods.isNotNull(response1)) {
					errorMessage = "Failed to obtain X-Midt-Partner-Id log message from parodus and webconfig log";
					partnerIdParodus = CommonMethods.patternFinder(response,
							BroadBandTestConstants.PATTERN_MATCHER_PARODUS_PARTNER_ID);
					partnerIdWebconfig = CommonMethods.patternFinder(response1,
							BroadBandTestConstants.PATTERN_MATCHER_WEBCONFIG_PARTNER_ID);
					if (CommonMethods.isNotNull(partnerIdParodus) && CommonMethods.isNotNull(partnerIdWebconfig)) {
						errorMessage = "Partner ID printed by Parodus in Parodus and webconfig log is different from partner ID parameter value";
						status = partnerIdParodus.equalsIgnoreCase(partnerId)
								&& partnerIdWebconfig.equalsIgnoreCase(partnerId);
					}
				}
			}

			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Partner ID printed in parodus and webconfig log are same as the one obtained from WebPa parameter");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s5";
			errorMessage = "Response code obtained is not as expected";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify issuer curl  response http_code 200 in Parodus & Webconfig log");
			LOGGER.info(
					"STEP 5: ACTION : Execute Command: grep -i \"themis curl response 0 http_code 200\" /rdklogs/logs/PARODUSlog.txt.0; grep -i \"ret = 0 http_code: 200\" /rdklogs/logs/WEBCONFIGlog.txt.0");
			LOGGER.info("STEP 5: EXPECTED : Token fetch must be success with response code : 200");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_PARODUS_HTTP_200, BroadBandCommandConstants.LOG_FILE_PARODUS,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			response1 = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_WEBCONFIG_HTTP_200,
					BroadBandCommandConstants.FILE_RDKLOGS_WEBCONFIG_LOG,
					BroadBandTestConstants.NINETY_SECOND_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(response1);
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Successfully obtained issuer curl response http_code 200 in Parodus and Webconfig log");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s6";
			errorMessage = "Curl response path log instance is not as expected";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify curl response path in Webconfig log");
			LOGGER.info(
					"STEP 6: ACTION : Execute Command : grep -i \"CURL_RESPONSE path is /tmp/.cURLresponse\" /rdklogs/logs/WEBCONFIGlog.txt.0");
			LOGGER.info("STEP 6: EXPECTED : Curl response path log instance must be expected");
			LOGGER.info("**********************************************************************************");

			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_WEBCONFIG_CURL_PATH,
					BroadBandCommandConstants.FILE_RDKLOGS_WEBCONFIG_LOG,
					BroadBandTestConstants.NINETY_SECOND_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (CommonMethods.isNull(response)) {
				response = tapEnv.searchAndFindLineWithMatchingStringFromStart(device,
						BroadBandTraceConstants.LOG_MESSAGE_WEBCONFIG_CURL_PATH,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			}
			status = CommonMethods.isNotNull(response);

			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Successfully verified curl response path in Webconfig log");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			LOGGER.info("**********************************************************************************");

			stepNum = "s7";
			errorMessage = "Token fetch failed when partner ID is empty";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify when partner ID is empty in Curl command token fetch is success");
			LOGGER.info("STEP 7: ACTION : Execute the Command");
			LOGGER.info(
					"STEP 7: EXPECTED : Token fetch must be success with response code : 200 when partner ID is empty");
			LOGGER.info("**********************************************************************************");

			deviceMacFromConfig = CommonUtils.getDeviceMacWithoutColon(device, tapEnv);
			if (CommonMethods.isNotNull(deviceMacFromConfig)) {
				command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.EXPORT_PATHS,
						BroadBandCommandConstants.CURL_CMD_HTTP, BroadBandCommandConstants.FILE_PATH_TMP_CURL_RESPONSE,
						BroadBandCommandConstants.OPTION_E, BroadBandCommandConstants.FILE_THEMIS_DECRYPTED_CLIENT_CERT,
						BroadBandCommandConstants.XMIDT_MAC_ADDRESS, deviceMacFromConfig,
						BroadBandCommandConstants.XMIDT_PARTNER_ID, partnerId, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
						BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
						BroadbandPropertyFileHandler.getParodusTokenServerURL());
				response = tapEnv.executeCommandInSettopBox(device, command);
				status = CommonMethods.isNotNull(response)
						&& CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_200)
						&& !CommonUtils.isGivenStringAvailableInCommandOutput(response,
								BroadBandTestConstants.TXT_INVALID_PARTNER_ID);
			}

			if (status) {
				LOGGER.info("STEP 7: ACTUAL : Successfully received the authentication token with partner id");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			LOGGER.info("**********************************************************************************");

			stepNum = "s8";
			errorMessage = "Unable to stop parodus service";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Kill parodus service running in device");
			LOGGER.info("STEP 8: ACTION : Execute Command : kill -11 pid , pidof parodus");
			LOGGER.info("STEP 8: EXPECTED : Parodus service must be stopped");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.killProcessAndVerify(device, tapEnv,
					BroadBandTestConstants.PROCESS_NAME_PARODUS, BroadBandTestConstants.PATTERN_PROCESS_PARODUS);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Successfully stopped parodus service");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s9";
			errorMessage = "Unable to restart parodus process";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Restart parodus with some invalid token server url");
			LOGGER.info("STEP 9: ACTION : Execute Command to restart parodus with invalid token server url");
			LOGGER.info("STEP 9: EXPECTED : Parodus must be restarted successfully");
			LOGGER.info("**********************************************************************************");

			if (CommonUtils.isGivenStringAvailableInCommandOutput(parodusCmd,
					BroadbandPropertyFileHandler.getParodusTokenServerURL())) {
				parodusCmd = parodusCmd.replace(BroadbandPropertyFileHandler.getParodusTokenServerURL(),
						BroadbandPropertyFileHandler.getInvalidParodusTokenServerURL());
				if (CommonMethods.isNotNull(parodusCmd)) {
					tapEnv.executeCommandUsingSsh(device, parodusCmd);
					tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
					status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
							BroadBandTestConstants.PROCESS_NAME_PARODUS);
				}
			}
			if (status) {
				LOGGER.info("STEP 9: ACTUAL : Parodus process was restarted successfully");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s10";
			errorMessage = "\"Failed to create new token\" log instance is not present in parodus log";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION : Verify \"Failed to create new token\" is present in parodus log");
			LOGGER.info(
					"STEP 10: ACTION : Execute Command : grep -i \"Failed to create new token\" /rdklogs/logs/PARODUSlog.txt.0");
			LOGGER.info(
					"STEP 10: EXPECTED : \"Failed to create new token\" log instance must be present in parodus log");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
					BroadBandTraceConstants.LOG_MESSAGE_THEMIS_TOKEN_FAILURE,
					BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : \"Failed to create new token\" log message is present in Parodus log as expected");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");

			stepNum = "s11";
			errorMessage = "Parodus process is not running successfully";
			status = false;

			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Verify parodus is running eventhough token fetch fails");
			LOGGER.info("STEP 11: ACTION : Execute command: pidof parodus");
			LOGGER.info("STEP 11: EXPECTED : Parodus must be running irrespective of token fetch");
			LOGGER.info("**********************************************************************************");

			status = CommonMethods
					.isNotNull(CommonUtils.getPidOfProcess(device, tapEnv, StbProcess.PARODUS.getProcessName()));
			if (status) {
				LOGGER.info("STEP 11: ACTUAL : Parodus process is up and running");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			LOGGER.info("**********************************************************************************");
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {

			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Reboot the device to remove invalid token server Url");
			LOGGER.info("POST-CONDITION : ACTION : Execute Command : reboot");
			LOGGER.info("POST-CONDITION : EXPECTED : Device must go on reboot ");

			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-1012");
	}
}
