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
package com.automatics.rdkb.tests.security;

import java.util.ArrayList;
/**
 * Test class with test case related to Security
 * 
 * @refactor Athira
 *
 */
import java.util.HashMap;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.testng.annotations.Test;
import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;

public class BroadBandDeviceFingerPrintTest extends AutomaticsTestBase {

	/**
	 * Test case is created as part of RDKB New feature AUTOMATION
	 *
	 * Test Case : Test to verify Rabid process up on boot
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1. Verify RABID process status</li>
	 * <li>Step 2. Verify RABID process should come up after reboot</li>
	 * <li>Step 3. Enable Device finger printing service using webpa</li>
	 * <li>Step 4. Enable safe browsing using webpa</li>
	 * <li>Step 5. Enabled softflowd using webpa</li>
	 * <li>Step 6. Disable Device finger printing service using webpa</li>
	 * <li>Step 7. Disable safe browsing using webpa</li>
	 * <li>Step 8. Disable softflowd using webpa</li>
	 * <li>Step 9. Verify RABID framework enable parameter has been removed</li>
	 * </ol>
	 * 
	 * @param device {@link Dut}
	 * 
	 * @author Leela Krishnama Naidu Andela
	 * @Refactor Athira
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-SECURITY-RM-LEGACY-1001")
	public void testToVerifyRabidProcessUpOnBoot(Dut device) {
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-SECURITY-RM-LEGACY-101";
		// Test step number
		int stepNumber = 1;
		String step = "S" + stepNumber;
		// String to store error message
		String errorMessage = null;
		String pidRabid = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY-RM-LEGACY-1001 ");
			LOGGER.info("TEST DESCRIPTION: Test to verify Advance security agent build ");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("Step 1. Verify RABID process status ");
			LOGGER.info("Step 2. Verify Rabid process should come up after reboot");
			LOGGER.info("Step 3. Enable Device finger printing service using webpa");
			LOGGER.info("Step 4. Enable safe browsing using webpa");
			LOGGER.info("Step 5. Enabled softflowd using webpa");
			LOGGER.info("Step 6. Disable Device finger printing using webpa");
			LOGGER.info("Step 7. Disable safe browsing using webpa");
			LOGGER.info("Step 8. Disable softflowd using webpa");
			LOGGER.info("Step 9. Verify RABID framework enable parameter has been removed");
			LOGGER.info("#######################################################################################");

			/**
			 * STEP 1 : VERIFY RABID PROCESS STATUS
			 */
			stepNumber = 1;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Verify RABID process status");
			LOGGER.info("STEP " + stepNumber + ": ACTION: Execute command: pidof rabid");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED: RABID process should be in running state");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the process id for rabid.";

			status = CommonMethods.isNotNull(tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_PID_OF,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.PROCESS_RABID)));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully verified the RABID process running status as true");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
			/**
			 * STEP 2 :
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Device could not be rebooted or not came up after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ " : DESCRIPTION : Reboot the device and verify Rabid process should come up after reboot");
			LOGGER.info("STEP " + stepNumber + " : ACTION : Execute Command: /sbin/reboot");
			LOGGER.info("STEP " + stepNumber
					+ " : EXPECTED : Device should reboot and come up and Rabid process should be running");
			LOGGER.info("**********************************************************************************");
			if (CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)) {
				errorMessage = "Unable to verify rabid process running";

				LOGGER.info("Waiting to start all the process");
				tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
				status = CommonMethods.isNotNull(tapEnv.executeCommandUsingSsh(device,
						BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_PID_OF,
								BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.PROCESS_RABID)));
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ " : ACTUAL : Successfully rebooted the device and Rabid process was running");
				} else {
					LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
				}
			} else {
				LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
			/**
			 * STEP 3 : VERIFY DEVICE FINGER PRINTING SERVICE USING WEBPA
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Verify Device finger printing service using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: Webpa should execute successfully and should enable device finger printing");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to enable device finger print using webpa set operation for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL: Successfully verified Device finger printing using webpa");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
			/**
			 * STEP 4: ENABLE SAFE BROWSING USING WEBPA
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Enable safe browsing using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: Webpa should execute successfully and should Enable safe browsing");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to enable safe browsing using webpa set operation for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully enabled safe browsing using webpa");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
			/**
			 * STEP 5 : ENABLED SOFTFLOWD USING WEBPA
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Enabled softflowd using webpa ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable ");
			LOGGER.info(
					"STEP " + stepNumber + ": EXPECTED: Webpa should execute successfully and should enable softflowd");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to enable softflowd using webpa set operation for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SOFTFLOWD, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully enabled softflowd using webpa");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
			/**
			 * STEP 6 : DISABLE DEVICE FINGER PRINTING SERVICE USING WEBPA
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Disable finger printing service using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: Webpa should execute successfully and should disable device finger printing");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to disable device finger print using webpa set operation for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL: Successfully verified Device finger printing using webpa");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
			/**
			 * STEP 7 : DISABLE SAFE BROWSING USING WEBPA
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Disable safe browsing using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: Webpa should execute successfully and should disable safe browsing");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to disable safe browsing using webpa set operation for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully disabled safe browsing using webpa");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
			/**
			 * STEP 8 : DISABLE SOFTFLOWD USING WEBPA
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Disable softflowd using webpa ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: Webpa should execute successfully and should disable softflowd");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to disable softflowd using webpa set operation for parameter Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SOFTFLOWD, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully disabled softflowd using webpa");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
			/**
			 * STEP 9 : Verify RABID framework enable parameter has been removed
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION: Verify RABID framework enable parameter has been removed ");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa or dmcli command to get&set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.Enable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED: Value must not be obtained for the parameter");
			LOGGER.info("******************************************************************************");
			errorMessage = "Able to get value for Rabid Enable parameter";
			status = CommonMethods.isNull(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_RABID_FRAME_WORK_ENABLE));
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL: Successfully verified value not obtained for the parameter");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}

			LOGGER.info("******************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while test to verify Rabid process up on boot " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY-RM-LEGACY-1001");
	}

	/**
	 *
	 * Test Case : Verify telemetry logging for Advanced security Cloud association
	 * success or failure and Advsec Status Enable or Disable, Advsec Agent Running
	 * or not, with configurable LoggingPeriod parameter
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>Step 1 : Get device time stamp and enable finger printing using webpa
	 * command.</li>
	 * <li>Step 2 : Verify default value of FingerPrint LoggingPeriod parameter</li>
	 * <li>Step 3 : Verify value greater than maximum limit of FingerPrint
	 * LoggingPeriod parameter cannot be set</li>
	 * <li>Step 4 : Verify setting maximum value of FingerPrint LoggingPeriod</li>
	 * <li>Step 5 : Verify value lesser than minimum limit of FingerPrint
	 * LoggingPeriod parameter cannot be set</li>
	 * <li>Step 6 : Verify setting minimum value of FingerPrint LoggingPeriod</li>
	 * <li>Step 7 : Verify setting default value of FingerPrint LoggingPeriod</li>
	 * <li>Step 8 : Modify interface using mount-copybind for Atom Sync Devices or
	 * deleting default interface for Non Atom Sync Devices.</li>
	 * <li>POST-CONDITION 1 : Reset LoggingPeriod parameter to default value if step
	 * 10 failed, Remove telemetry configurations and reboot.</li>
	 * </ol>
	 * 
	 * @author Ashwin sankara
	 * @author Muthukumar
	 * @refactor Govardhan
	 * @param device {@link Dut}
	 * 
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.TELEMETRY)
	@TestDetails(testUID = "TC-RDKB-FINGERPRINT-1005")
	public void testVerifyDeviceFingerPrintTelemetry(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FINGERPRINT-105";
		int stepNumber = BroadBandTestConstants.CONSTANT_1;
		String step = "S" + stepNumber;
		String errorMessage = null;
		String response = null;
		String deviceTimeStamp = null;
		long startTime = BroadBandTestConstants.CONSTANT_0;
		boolean status = false;
		boolean isPostConditionNeeded = false;
		ArrayList<String> verifySplunkLog = new ArrayList<>();
		// Variable Declaration Ends
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-FINGERPRINT-1005");
			LOGGER.info(
					"TEST DESCRIPTION: Verify telemetry logging for Advanced security Cloud association success or failure and Advsec "
							+ "Status Enable or Disable, Advsec Agent Running or not, with configurable LoggingPeriod parameter");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("1. Get device time stamp and enable finger printing using webpa command.");
			LOGGER.info("2. Verify default value of FingerPrint LoggingPeriod parameter");
			LOGGER.info(
					"3. Verify value greater than maximum limit of FingerPrint LoggingPeriod parameter cannot be set");
			LOGGER.info("4. Verify setting maximum value of FingerPrint LoggingPeriod ");
			LOGGER.info(
					"5. Verify value lesser than minimum limit of FingerPrint LoggingPeriod parameter cannot be set");
			LOGGER.info("6. Verify setting minimum value of FingerPrint LoggingPeriod ");
			LOGGER.info("7. Verify setting default value of FingerPrint LoggingPeriod");
			LOGGER.info(
					"8. Modify interface using mount-copybind for Atom Sync Devices or deleting default interface for Non Atom Sync Devices.");
			LOGGER.info(
					" POST-CONDITION 1 : Reset LoggingPeriod parameter to default value if step 10 failed, Remove telemetry configurations and reboot.");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * STEP 1 : CONFIGURE MOCK TELEMETRY PROFILE FOR ADVSEC CLOUD ASSOC SUCCESS AND
			 * FAILURE MARKERS AND REBOOT DUT.
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"PRE-CONDITION 1 : DESCRIPTION : Configure mock telemetry profile for Advsec cloud assoc success and failure markers and reboot DUT.");
			LOGGER.info(
					"PRE-CONDITION 1 : ACTION : Copy and update dcm.properties in /nvram and Post telemetry profile to mock dcm server.");
			LOGGER.info(
					"PRE-CONDITION 1  : EXPECTED : Successfully copied, updated /nvram/dcm.properties and posted telemetry payload data.");
			LOGGER.info("#######################################################################################");
			try {
				BroadBandTelemetryUtils.configureTelemetryProfiles(device, tapEnv, true);
				BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
				status = true;
			} catch (Exception e) {
				errorMessage = BroadBandTestConstants.PRE_CONDITION_ERROR + e.getMessage();
			}

			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
				throw new TestException(errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			/**
			 * STEP 1 : GET DEVICE TIME STAMP AND VERIFY FINGER PRINTING USING WEBPA COMMAND
			 */
			stepNumber = 1;
			step = "S" + stepNumber;
			errorMessage = "Unable to get time stamp from device after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Get device time stamp and enable finger printing using webpa command.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to get Device.DeviceInfo.X_RDKCENTRAL-COM_DeviceFingerPrint.Enable as true");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa get successful and parameter value should be true");
			LOGGER.info("**********************************************************************************");
			deviceTimeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
			if (CommonMethods.isNotNull(deviceTimeStamp)) {
				errorMessage = "Unable to set finger print parameter to true";
				status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_ENABLE, BroadBandTestConstants.TRUE,
						BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Webpa get successful and parameter value is true");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

			/**
			 * STEP 2 : VERIFY DEFAULT VALUE OF FINGERPRINT LOGGINGPERIOD PARAMETER
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Value of FingerPrint LoggingPeriod parameter is not 1440 by default";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify default value of FingerPrint LoggingPeriod parameter");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to get value of FingerPrint LoggingPeriod parameter");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : WebPA get request success and FingerPrint LoggingPeriod parameter value is 1440");
			LOGGER.info("**********************************************************************************");
			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD);
			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_1440);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : WebPA get request success and FingerPrint LoggingPeriod parameter value is 1440");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * STEP 3 : VERIFY VALUE GREATER THAN MAXIMUM LIMIT OF FINGERPRINT LOGGING
			 * PERIOD PARAMETER CANNOT BE SET
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Setting of FingerPrint LoggingPeriod parameter with value greater than max limit is success";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify value greater than maximum limit of FingerPrint LoggingPeriod parameter cannot be set");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to set FingerPrint LoggingPeriod parameter to 7895 value");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPA request failed to set value greater than max limit");
			LOGGER.info("**********************************************************************************");
			status = !BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD,
					BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_VALUE_SEVEN_THOUSAND_EIGHT_HUNDRED_AND_NINETY_FIVE);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : WebPA request failed to set value greater than max limit");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * STEP 4 : VERIFY SETTING MAXIMUM VALUE OF FINGERPRINT LOGGINGPERIOD
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Unable to set FingerPrint LoggingPeriod parameter value to max value 2880";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify setting maximum value of FingerPrint LoggingPeriod");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to set FingerPrint LoggingPeriod parameter to 2880 value");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : WebPA set request success and FingerPrint LoggingPeriod parameter value is 2880");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD,
					BroadBandTestConstants.CONSTANT_2,
					BroadBandTestConstants.STRING_VALUE_TWO_THOUSAND_EIGHT_HUNDRED_EIGHTY);
			// Post condition needed in case of successful change from default value
			isPostConditionNeeded = status;
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : WebPA set request success and FingerPrint LoggingPeriod parameter value is 2880");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * STEP 5 : VERIFY VALUE LESSER THAN MINIMUM LIMIT OF FINGERPRINT LOGGINGPERIOD
			 * PARAMETER CANNOT BE SET
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Setting of FingerPrint LoggingPeriod parameter with value lesser than min limit is success";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify value lesser than minimum limit of FingerPrint LoggingPeriod parameter cannot be set");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to set FingerPrint LoggingPeriod parameter to 30 value");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : WebPA request failed to set value lesser than min limit");
			LOGGER.info("**********************************************************************************");
			status = !BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD,
					BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_30);
			if (status) {
				LOGGER.info(
						"STEP " + stepNumber + ": ACTUAL : WebPA request failed to set value lesser than min limit");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * STEP 6 : VERIFY SETTING MINIMUM VALUE OF FINGERPRINT LOGGINGPERIOD
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Unable to set FingerPrint LoggingPeriod parameter value to min value 60";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify setting minimum value of FingerPrint LoggingPeriod");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to set FingerPrint LoggingPeriod parameter to 60 value");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : WebPA set request success and FingerPrint LoggingPeriod parameter value is 60");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD,
					BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_60);
			// Post condition needed in case of successful change from default value
			isPostConditionNeeded = status;
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : WebPA set request success and FingerPrint LoggingPeriod parameter value is 60");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * STEP 7 : VERIFY SETTING DEFAULT VALUE OF FINGERPRINT LOGGINGPERIOD
			 */
			stepNumber++;
			step = "S" + stepNumber;
			errorMessage = "Unable to set FingerPrint LoggingPeriod parameter value to default value 1440";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP " + stepNumber + ": DESCRIPTION : Verify setting default value of FingerPrint LoggingPeriod");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command to set FingerPrint LoggingPeriod parameter to 1440 value");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : WebPA get request success and FingerPrint LoggingPeriod parameter value is 1440");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD,
					BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_1440);
			// Post condition not needed in case of successful change to default value
			isPostConditionNeeded = !status;
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : WebPA set request success and FingerPrint LoggingPeriod parameter value is 60");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

			/**
			 * STEP 8 : MODIFY INTERFACE USING MOUNT-COPYBIND FOR Atom Sync Devices OR
			 * DELETING DEFAULT INTERFACE FOR Non Atom Sync Devices
			 */
			stepNumber++;
			step = "S" + stepNumber;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Modify interface using mount-copybind for Atom Sync Devices or deleting default interface for Non Atom Sync Devices.");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute commands: copy, sed, mount-copybind for Atom Sync Devices or ip route del default for Non Atom Sync Devices");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Interface has been modified to simulate cloud connect fail for finger printing");
			LOGGER.info("**********************************************************************************");
			if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
				errorMessage = "Unable to replace interface name in advsec.sh using mount-copybind";
				status = BroadBandCommonUtils.copyReplaceTextMountCopyBindRemoveFile(device, tapEnv,
						BroadBandCommandConstants.FILE_PATH_ADVSEC_SH, BroadBandTestConstants.NVRAM_PATH,
						BroadBandTestConstants.STRING_EXPORT, BroadBandTestConstants.STRING_INVALID);
			} else {
				errorMessage = "Unable to delete default interface from iproute";
				tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_DEL_DEFAULT_IP_ROUTE);
				// Checking null value obtained by grep of default iproute interface after
				// deleting to pass the step
				status = CommonMethods.isNull(
						tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GREP_DEFAULT_IP_ROUTE));
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Interface has been modified to simulate cloud connect fail for finger printing");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		} catch (Exception e) {
			errorMessage = e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, false, errorMessage, false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			/**
			 * POST CONDITION 1 : RESET LOGGINGPERIOD PARAMETER TO DEFAULT VALUE IF STEP 10
			 * FAILED, REMOVE TELEMETRY CONFIGURATIONS AND REBOOT
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info(
					"POST-CONDITION 1 : DESCRIPTION : Reset LoggingPeriod parameter to default value if step 10 failed, Remove telemetry configurations and reboot.");
			LOGGER.info(
					"POST-CONDITION 1 : ACTION : Set FingerPrint LoggingPeriod parameter to 1440 value, remove telemetry configuration files and reboot");
			LOGGER.info("POST-CONDITION 1 : EXPECTED : Post condition executed successfully");
			LOGGER.info("#######################################################################################");
			// Logging period parameter is only accessible when finger print enabled, so we
			// enable in order to reset
			// default value of Logging period
			if (isPostConditionNeeded) {
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_FINGER_PRINT_LOGGING_PERIOD,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_1440);
			} else {
				LOGGER.info("FingerPrint LoggingPeriod already set to default value");
				status = true;
			}
			errorMessage = "Unable to reset FingerPrint LoggingPeriod parameter to default value 1440";
			if (status) {
				errorMessage = "Unable to clear telemetry config files from /nvram/ and /tmp/";
				if (BroadBandTelemetryUtils.clearTelemetryConfiguration(tapEnv, device)) {
					errorMessage = "Unable to reboot device successfully";
					status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
				}
			}
			if (status) {
				LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
			} else {
				LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed: " + errorMessage);
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FINGERPRINT-1005");
	}

	/**
	 * Test to verify OVS functionality
	 * <li>1. Update OVS status to disable using webpa</li>
	 * <li>2. Verify log message for OVS disabled in MeshAgent log file</li>
	 * <li>3. Update the OVS enable status as true using RFC</li>
	 * <li>4. Verify OVS enable status using webpa after RFC update</li>
	 * <li>5. Verify log message for OVS enabled in MeshAgent log file</li>
	 * <li>6. Verify OVS enabled status in ARM console</li>
	 * <li>7. Verify OVS parameter details in ARM console</li>
	 * <li>8. Verify OVS enabled in ATOM console</li>
	 * <li>9. Verify OVS parameter details in ATOM console</li>
	 * 
	 * @author Arunkumar Jayachandran
	 * @Refactor Sruthi Santhosh
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			TestGroup.NEW_FEATURE, TestGroup.SECURITY })
	@TestDetails(testUID = "TC-RDKB-OVS-1001")
	public void testToVerifyOVS(Dut device) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-OVS-1001");
		LOGGER.info("TEST DESCRIPTION: Test to verify OVS functionality");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update OVS status to disable using webpa");
		LOGGER.info("2. Verify log message for OVS disabled in MeshAgent log file");
		LOGGER.info("3. Update the OVS enable status as true using RFC");
		LOGGER.info("4. Verify OVS enable status using webpa after RFC update");
		LOGGER.info("5. Verify log message for OVS enabled in MeshAgent log file");
		LOGGER.info("6. Verify OVS enabled status in ARM console");
		LOGGER.info("7. Verify OVS parameter details in ARM console");
		LOGGER.info("8. Verify OVS enabled in ATOM console");
		LOGGER.info("9. Verify OVS parameter details in ATOM console");
		LOGGER.info("#######################################################################################");

		// variable declaration begins
		// Status of test script verification
		boolean status = false;
		// Test case id
		String testCaseId = "TC-RDKB-OVS-101";
		// Test step number
		String stepNumber = "s1";
		// String to store error message
		String errorMessage = null;
		// String to store response
		String response = null;
		// variable declaration ends

		try {
			stepNumber = "s1";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Update OVS status to disable using webpa");
			LOGGER.info(
					"STEP 1: ACTION: Execute webpa set command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable bool false");
			LOGGER.info("STEP 1: EXPECTED: OVS enable status should be disabled");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the OVS enable status using webpa";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_OVS_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.FALSE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Successfully updated OVS enable status as false");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s2";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Verify log message for OVS disabled in MeshAgent log file");
			LOGGER.info(
					"STEP 2: ACTION: Execute command in ATOM console: grep -i ovs /rdklogs/logs/MeshAgentLog.txt.*");
			LOGGER.info("STEP 2: EXPECTED: Should get the log message for OVS disabled in MeshAgent log file");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message for OVS disabled in MeshAgent log file";
			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_OVS, BroadBandCommandConstants.LOG_FILE_MESHAGENT,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_OVS_DISABLED);
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL: Successfully verified log message for OVS disabled status in Meshagent log file");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s3";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Update the OVS enable status as true using RFC");
			LOGGER.info(
					"STEP 3: ACTION: Configure RFC: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable as true");
			LOGGER.info("STEP 3: EXPECTED: OVS enable status should be enabled using RFC");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the OVS enable status as true using RFC";
			status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
					BroadBandTestConstants.FEATURE_NAME_OVS, true);
			if (status) {
				LOGGER.info("STEP 3: ACTUAL: Successfully updated OVS as true using RFC");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			// ##################################################################################################//

			stepNumber = "s4";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Verify OVS enable status using webpa after RFC update");
			LOGGER.info(
					"STEP 4: ACTION: Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable");
			LOGGER.info("STEP 4: EXPECTED: OVS enable should be true");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_OVS_ENABLE, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL: Successfully verified OVS enable status as true using webpa after RFC update");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s5";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Verify log message for OVS enabled in MeshAgent log file");
			LOGGER.info(
					"STEP 5: ACTION: Execute command in ATOM console: grep -i ovs /rdklogs/logs/MeshAgentLog.txt.*");
			LOGGER.info("STEP 5: EXPECTED: Should get the log message for OVS enabled in MeshAgent log file");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to get the log message for OVS enabled in MeshAgent log file";
			response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
					BroadBandTraceConstants.LOG_MESSAGE_OVS, BroadBandCommandConstants.LOG_FILE_MESHAGENT,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_OVS_ENABLED);
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL: Successfully verified log message for OVS enabled status in Meshagent log file");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s6";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION: Verify OVS enabled status in ARM console");
			LOGGER.info("STEP 6: ACTION: Execute command in ARM console: ovs-vsctl show");
			LOGGER.info("STEP 6: EXPECTED: OVS should not be enabled ARM console");
			LOGGER.info("******************************************************************************");
			errorMessage = "OVS enaled in ARM console";
			BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.OVS_SHOW_COMMAND);
			status = CommonMethods.isNotNull(response) && !CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_IS_CONNECTED);
			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Successfully verified ovs is not enabled in ARM console");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s7";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Verify OVS parameter details in ARM console");
			LOGGER.info("STEP 7: ACTION: Execute command in ARM console:"
					+ "cat /sys/module/acc/parameters/ovs_wifi_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_wifi_miss_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_miss_count"
					+ "cat /sys/module/acc/parameters/ovs_wifi_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_wifi_miss_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_miss_count");
			LOGGER.info("STEP 7: EXPECTED: Should not get the OVS parameter details from ARM console");
			LOGGER.info("******************************************************************************");
			errorMessage = "OVS is enabled in ARM console";
			response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_OVS_PARAMETERS);
			status = CommonMethods.isNotNull(response) && (CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY));
			if (status) {
				LOGGER.info("STEP 7: ACTUAL: Successfully verified OVS parameters are not present in ARM console");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s8";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Verify OVS enabled status in ATOM console");
			LOGGER.info("STEP 8: ACTION: Execute command in ATOM console: ovs-vsctl show");
			LOGGER.info("STEP 8: EXPECTED: OVS should be enabled ATOM console");
			LOGGER.info("******************************************************************************");
			errorMessage = "OVS not enaled in ATOM console";
			response = BroadBandCommonUtils.executeCommandInAtomConsoleByPolling(tapEnv, device,
					BroadBandCommandConstants.OVS_SHOW_COMMAND, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
					BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
					BroadBandTraceConstants.LOG_MESSAGE_IS_CONNECTED);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL: Successfully verified ovs enabled in ATOM console");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

			stepNumber = "s9";
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION: Verify OVS parameter details in ATOM console");
			LOGGER.info("STEP 9: ACTION: Execute command in ATOM console:"
					+ "cat /sys/module/acc/parameters/ovs_wifi_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_wifi_miss_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_miss_count"
					+ "cat /sys/module/acc/parameters/ovs_wifi_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_wifi_miss_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_hit_count"
					+ "cat /sys/module/acc/parameters/ovs_udma_miss_count");
			LOGGER.info("STEP 9: EXPECTED: Should get the OVS parameter details from ATOM console");
			LOGGER.info("******************************************************************************");
			errorMessage = "OVS is not enabled in ATOM console";
			response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
					BroadBandCommandConstants.CMD_OVS_PARAMETERS);
			status = CommonMethods.isNotNull(response)
					&& !(CommonUtils.isGivenStringAvailableInCommandOutput(response,
							BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY))
					&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCHER_GET_NUMBER);
			if (status) {
				LOGGER.info("STEP 9: ACTUAL: Successfully verified OVS parameters are present in ATOM console");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
			// ##################################################################################################//

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception Occurred while Verifying OVS enable in ATOM console" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			LOGGER.info("POST-CONDITION : DESCRIPTION : Remove RFC configuration from the Xconf server");
			LOGGER.info(
					"POST-CONDITION : ACTION : <XCONF-URL>/featureControl/clear?estbMacAddress=<mac>&featureName=OVS");
			LOGGER.info("POST-CONDITION : EXPECTED : Should clear the configuration from Xconf server");
			status = (HttpStatus.SC_OK == BroadBandRfcFeatureControlUtils.clearSettingsInProxyXconfDcmServerForRDKB(
					device, tapEnv, false, BroadBandTestConstants.FEATURE_NAME_OVS));
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-OVS-1001");
		// ###############################################################//
	}

}
