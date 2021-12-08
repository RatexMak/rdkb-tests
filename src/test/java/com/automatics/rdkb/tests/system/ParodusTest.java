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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.reboot.BootTime.BootTimePatterns;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ParodusUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

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
     * <li>Step 1 : Obtain wan_init_complete time in ArmConsolelog for Atom Sync Available devices and Consolelog for other platforms</li>
     * <li>Step 2 : Verify parodus process is running</li>
     * <li>Step 3 : Obtain parodus starting time from timestamp in log message in PARODUSlog</li>
     * <li>Step 4 : Verify parodus is connected to server over SSL log message in PARODUSlog</li>
     * <li>Step 5 : Obtain parodus connected time from log message in PARODUSlog</li>
     * <li>Step 6 : Obtain WebPA starting time from timestamp in log message in WEBPAlog</li>
     * <li>Step 7 : Obtain boot_to_WEBPA_READY_uptime value from log message in WEBPAlog</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
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
	     * STEP 1 : OBTAIN WAN_INIT_COMPLETE TIME IN ARMCONSOLELOG FOR ATOM SYNC AVAILABLE DEVICES AND CONSOLELOG FOR OTHER PLATFORMS
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
	     * STEP 2 : OBTAIN BOOT_TO_WEBPA_READY_UPTIME VALUE FROM LOG MESSAGE IN BootTime.log
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
	     * STEP 4 : OBTAIN PARODUS STARTING TIME FROM TIMESTAMP IN LOG MESSAGE IN PARODUSLOG
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
	     * STEP 5 : VERIFY PARODUS IS CONNECTED TO SERVER OVER SSL LOG MESSAGE IN PARODUSLOG
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
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION :Execute webpa command Device.DeviceInfo.Manufacturer");
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
}
