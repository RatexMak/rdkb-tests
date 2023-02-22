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
import java.util.List;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.codehaus.jettison.json.JSONException;
import org.testng.annotations.Test;
import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWifiWhixUtils;

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
	
	/**
    *
    * Test Case : Test to integrate OVS in RDKB-Productization
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>Step 1 : Get and verify the brlan0 bridge name</li>
    * <li>Step 2 : Get the list of interface names associated with brlan0</li>
    * <li>Step 3 : Update the OVS enable status as true using RFC</li>
    * <li>Step 4 : Verify OVS enable status using webpa after RFC update</li>
    * <li>Step 5 : Verify the log information in OvsAgentApi.log</li>
    * <li>Step 6 : Verify the log information in OvsAgentLog.txt.0</li>
    * <li>Step 7 : Verify the log information in bridgeUtils.log</li>
    * <li>Step 8 : Verify that device logs the time required for completing the intermediate stages in BootTime.log
    * </li>
    * <li>Step 9 : Verify log message for OVS enabled in MeshAgent log file</li>
    * <li>Step 10 : Verify Vlan bridges when ovs was enabled under OVS bridge control and verify the interface details
    * for brlan0 from Step #2</li>
    * <li>Step 11 : Verify bridge and vlan command interface under linux bridge control</li>
    * <li>Step 12 : Verify Collecting CPU and memory usage stats for 5mins when OVS is enabled.</li>
    * <li>Step 13 : set and verify Advance Security to be enabled</li>
    * <li>Step 14 : Verify the flood treshold to tcp and upd packets</li>
    * <li>Step 15 : Update the OVS disabled status as true using RFC</li>
    * <li>Step 16 : Verify that device logs the time required for completing the intermediate stages in BootTime.log
    * </li>
    * <li>Step 17 : Compare the boot time log before Ovs enable and after enabled</li>
    * <li>Step 18 : Verify log message for OVS disabled in MeshAgent log file</li>
    * <li>Step 19 : Verify Collecting CPU and memory usage stats for 5mins when Ovs is disabled.</li>
    * <li>Step 20 : Verify negative impact of enabling OVS on CPU and Memory</li>
    * <li>Step 21 : Verify Vlan bridges when OVS was disabled under OVS bridge control</li>
    * <li>Step 22 : Verify bridge and vlan command interface under linux bridge control</li>
    * <li>Post Condition : Remove RFC configuration from the Xconf server</li>
    * </ol>
    * 
    * @param device
    *            {@link Instance of Dut}
    * 
    * @author Lakshmi Keshava Murthy, Muthukumar
    * @refactor yamini.s
    *
    */
   @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
   	    TestGroup.NEW_FEATURE, TestGroup.SECURITY })
       @TestDetails(testUID = "TC-RDKB-OVS-1002")
   public void testToVerifyOVSintegration(Dut device) throws JSONException {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-OVS-102";
	boolean status = false;
	String response = null;
	String errorMessage = null;
	String beforeEnablingFeature = null;
	String afterEnablingFeature = null;
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	String stepNum = "s" + stepNumber;
	// Variable Declaration Ends
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-OVS-1002");
	    LOGGER.info("TEST DESCRIPTION: Test to integrate OVS in RDKB-Productization");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Step 1 : Get and verify the brlan0 bridge name");
	    LOGGER.info("Step 2 : Get the list of interface names associated with brlan0");
	    LOGGER.info("Step 3 : Update the OVS enable status as true using RFC");
	    LOGGER.info("Step 4 : Verify OVS enable status using webpa after RFC update");
	    LOGGER.info("Step 5 : Verify the log information in OvsAgentApi.log");
	    LOGGER.info("Step 6 : Verify the log information in OvsAgentLog.txt.0");
	    LOGGER.info("Step 7 : Verify the log information in bridgeUtils.log");
	    LOGGER.info(
		    "Step 8 : Verify that device logs the time required for  completing  the intermediate stages in BootTime.log");
	    LOGGER.info("Step 9 : Verify log message for OVS enabled in MeshAgent log file");
	    LOGGER.info(
		    "Step 10 : Verify Vlan bridges when ovs was enabled under OVS bridge control and verify the interface details for brlan0 from Step #2");
	    LOGGER.info("Step 11 : Verify bridge and vlan command interface under linux bridge control");
	    LOGGER.info("Step 12 : Verify Collecting CPU and memory usage stats for 5mins when OVS is enabled.");
	    LOGGER.info("Step 13 : set and verify Advance Security to be enabled");
	    LOGGER.info("Step 14 : Verify the flood treshold to tcp and upd packets");
	    LOGGER.info("Step 15 : Update the OVS disabled status as true using RFC");
	    LOGGER.info(
		    "Step 16 : Verify that device logs the time required for  completing  the intermediate stages in BootTime.log");
	    LOGGER.info("Step 17 : Compare the boot time log before Ovs enable and after enabled");
	    LOGGER.info("Step 18 : Verify log message for OVS disabled in MeshAgent log file");
	    LOGGER.info("Step 19 : Verify Collecting CPU and memory usage stats for 5mins when Ovs is disabled.");
	    LOGGER.info("Step 20 : Verify negative impact of enabling OVS on CPU and Memory");
	    LOGGER.info("Step 21 : Verify Vlan bridges when OVS was disabled under OVS bridge control");
	    LOGGER.info("Step 22 : Verify bridge and vlan command interface under linux bridge control");
	    LOGGER.info("Post Condition : Remove RFC configuration from the Xconf server");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1 : GET AND VERIFY THE BRLAN0 BRIDGE NAME
	     */
	    errorMessage = "Failed to get and verify the brlan0 bridge name ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Get and verify the brlan0 bridge name");
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION : Execute command: psmcli get dmsb.l2net.1.Port.1.Name – brlan0");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Must return the brlan0 bridge name");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_PSMCLI_GET_DMSB_L2NET_1_PORT_1_NAME);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.patternSearchFromTargetString(response,
		    BroadBandTestConstants.INTERFACE_NAME_BRLAN0);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully Get and verified the brlan0 bridge name");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 2 : GET THE LIST OF INTERFACE NAMES ASSOCIATED WITH BRLAN0
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the list of interface names associated with brlan0 ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Get the list of interface names associated with brlan0");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: grep -i \"dmsb.l2net.1.Members.\" /tmp/bbhm_cur_cfg.xml");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Must return the list of interface names associated with brlan0");
	    LOGGER.info("**********************************************************************************");
	    List<String> listOfInterfaces = BroadBandCommonUtils.getListOfInterfaces(device, tapEnv);
	    status = !listOfInterfaces.isEmpty();
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Sucessfully retrieved the list of interface names associated with brlan0");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 3 : UPDATE THE OVS ENABLE STATUS AS TRUE USING RFC
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to update the OVS enable status as true using RFC";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Update the OVS enable status as true using RFC");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable as true");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : OVS enable status should be enabled using RFC");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.OVS_FEATURE_NAME, true);
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + ": ACTUAL : Sucessfully updated OVS enable status as true using RFC");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 4 : VERIFY OVS ENABLE STATUS USING WEBPA AFTER RFC UPDATE
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the response for webpa parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify OVS enable status using webpa after RFC update");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION :Execute webpa command : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED :  OVS enable should be true");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_OVS_ENABLE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified OVS enable status using webpa after RFC update");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 5 : VERIFY THE LOG INFORMATION IN OVSAGENTAPI.LOG
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to verify the log information in OvsAgentApi.log";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify the log information in OvsAgentApi.log ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: 1. grep -i \"open_log successfully opened\"  /rdklogs/logs/OvsAgentApi.log\n"
		    + "2. grep -i \"Socket connected successfully\"  /rdklogs/logs/OvsAgentApi.log\n"
		    + "3. grep -i \"ovs_agent_api_init successfully initialized\"  /rdklogs/logs/OvsAgentApi.log\n"
		    + "4. grep -i \"ovsdb_write successfully wrote\"  /rdklogs/logs/OvsAgentApi.log\n"
		    + "5. grep -i \"ovsdb_monitor successfully wrote\"  /rdklogs/logs/OvsAgentApi.log\n"
		    + "6. grep -i \"handle_transact_insert_request\"  /rdklogs/logs/OvsAgentApi.log");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : Must return the expected log information in OvsAgentApi.log");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.validateListOfLogInformation(device, tapEnv,
		    BroadBandTraceConstants.OVSAGENTAPI_LOG, BroadBandCommandConstants.FILE_PATH_OVSAGENTAPI_LOG);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Sucessfully verified the log information in OvsAgentApi.log ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 6 : VERIFY THE LOG INFORMATION IN OVSAGENTLOG.TXT.0
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to verify the log information in OvsAgentApi.log";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify the log information in OvsAgentLog.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: 1.grep -i \"Ovs Agent Log Initialized\"  /rdklogs/logs/OvsAgentLog.txt.0\n"
		    + "2.grep -i \"Ovs Agent Api Initialized for Component\"  /rdklogs/logs/OvsAgentLog.txt.0\n"
		    + "3.grep -i \"Ovs Action Initialized\"  /rdklogs/logs/OvsAgentLog.txt.0\n"
		    + "4.grep -i \"Ovs Agent interact monitor table 0 succeeded\"  /rdklogs/logs/OvsAgentLog.txt.0\n"
		    + "5.grep -i \"gwconf_mon_cb interact transact table 1 succeeded\"  /rdklogs/logs/OvsAgentLog.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Must return the expected log information in OvsAgentLog.txt.0");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.validateListOfLogInformation(device, tapEnv,
		    BroadBandTraceConstants.OVSAGENTLOG_TXT, BroadBandCommandConstants.FILE_PATH_OVSAGENTLOG_TXT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Sucessfully verified the log information in OvsAgentLog.txt.0");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 7 : VERIFY THE LOG INFORMATION IN BRIDGEUTILS.LOG
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to verify the log information in bridgeUtils.log";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify the log information in bridgeUtils.log");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command: 1. grep -i \"updateBridgeInfo : parent_ifname is  : if_name is <INTERFACE_NAME> : bridge <BRIDGE_NAME>\"  /rdklogs/logs/bridgeUtils.log\n"
		    + "2. grep -i \"create_bridge_api ovs is enabled, calling ovs api\"  /rdklogs/logs/bridgeUtils.log");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : Must return the expected log information in bridgeUtils.log");
	    LOGGER.info("**********************************************************************************");
	    String command = null;
	    String logMessage = null;
	    int resultCount = BroadBandTestConstants.CONSTANT_0;
	    try {
		for (String interfaceName : listOfInterfaces) {
		    logMessage = BroadBandTraceConstants.LOG_MESSAGE_UPDATEB_RIDGE_INFO.replace(
			    BroadBandTestConstants.STRING_INTERFACE_TO_REPLACE,
			    BroadBandTestConstants.INTERFACE_NAME_BRLAN0);
		    logMessage = logMessage.replace(BroadBandTestConstants.STRING_REPLACE, interfaceName);
		    command = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandCommandConstants.CMD_GREP_A_I.replaceAll(
				    BroadBandTestConstants.STRING_VALUE_TO_REPLACE,
				    BroadBandTestConstants.STRING_CONSTANT_1),
			    BroadBandTestConstants.TEXT_DOUBLE_QUOTE, logMessage,
			    BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandCommandConstants.FILE_PATH_BRIDGEUTILS_LOG);
		    response = tapEnv.executeCommandUsingSsh(device, command);
		    if (CommonMethods.isNotNull(response) && BroadBandCommonUtils.patternSearchFromTargetString(
			    response, BroadBandTraceConstants.LOG_MESSAGE_CREATE_BRIDGE_API)) {
			resultCount = resultCount + BroadBandTestConstants.CONSTANT_1;
		    } else {
			LOGGER.error("Failed to verify " + command);
		    }
		}
	    } catch (Exception e) {
		errorMessage += e.getMessage();
	    }
	    status = listOfInterfaces.size() == resultCount;
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Sucessfully verified the log information in bridgeUtils.log");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 8 : VERIFY THAT DEVICE LOGS THE TIME REQUIRED FOR COMPLETING THE INTERMEDIATE STAGES IN BOOTTIME.LOG
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to capture the boot time logs";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify that device logs the time required for  completing  the intermediate stages in BootTime.log ");
	    LOGGER.info("STEP " + stepNumber
		    + "ACTION : Execute the command in device console: cat /rdklogs/logs/BootTime.log to get the boot time values");
	    LOGGER.info("STEP " + stepNumber
		    + "EXPECTED : Device should log the time take for completing intermediate stages");
	    LOGGER.info("**********************************************************************************");
	    HashMap<String, Integer> bootTimeAfterOvs = new HashMap<String, Integer>();
	    try {
		for (String process : BroadBandCommandConstants.BOOTTIME_COMPONENT_LIST_OVS) {
		    String logsAfterOvs = BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device, process,
			    BroadBandCommandConstants.FILE_BOOT_TIME_LOG);
		    status = CommonMethods.isNotNull(logsAfterOvs);
		    if (status) {
			int uptimeAfterOvs = Integer.parseInt(logsAfterOvs.split("=")[1]);
			bootTimeAfterOvs.put(process, uptimeAfterOvs);
		    }
		    status = bootTimeAfterOvs.size() == 8;
		}
	    } catch (TestException e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully captured boot time logs");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 9 : VERIFY LOG MESSAGE FOR OVS ENABLED IN MESHAGENT LOG FILE
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the log message for OVS enabled in MeshAgent log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :  Verify log message for OVS enabled in MeshAgent log file");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command in ATOM console : grep -i ovs /rdklogs/logs/MeshAgentLog.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED :  Should get the log message for OVS enabled in MeshAgent log file");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_OVS, BroadBandCommandConstants.LOG_FILE_MESHAGENT,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTraceConstants.LOG_MESSAGE_OVS_ENABLED_MESH);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully received the log message for OVS enabled in MeshAgent log file");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 10 : VERIFY VLAN BRIDGES WHEN OVS WAS ENABLED UNDER OVS CONTROL BRIDGE
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to valiadte the VLAN bridge status after OVS enabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify VLAN bridges when OVS was enabled under OVS control bridge");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command: ovs-vsctl show");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ovs vlan control having newly created VLAN bridges");
	    LOGGER.info("************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.OVS_SHOW_COMMAND);
		resultCount = BroadBandTestConstants.CONSTANT_0;
		if (CommonMethods.isNotNull(response)
			&& BroadBandCommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.STRING_PORT_BRLAN0)
			&& BroadBandCommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.STRING_BRIDGE_BRLAN0)) {
		    for (String interfaceName : listOfInterfaces) {
			if (BroadBandCommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.PORT_NAME.replace(BroadBandTestConstants.STRING_REPLACE,
					interfaceName))
				&& BroadBandCommonUtils.patternSearchFromTargetString(response,
					BroadBandTestConstants.INTERFACE_NAME
						.replace(BroadBandTestConstants.STRING_REPLACE, interfaceName))) {
			    resultCount = resultCount + BroadBandTestConstants.CONSTANT_1;
			}
		    }
		}
		status = listOfInterfaces.size() == resultCount;
	    } catch (Exception e) {
		errorMessage += e.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfuly validated VLAN bridges when OVS enabled ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 11 : Verify bridge and VLAN command interface under linux bridge control
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    errorMessage = "Failed to validate the VLAN bridge status empty after OVS enabled";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify bridge and VLAN command interface under linux bridge control");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Execute linux Command: brctl show");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Bridge and Vlan details should be empty under bridge control after OVS enabled");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.BRCTL_SHOW);
	    status = CommonMethods.isNotNull(response)
		    && !CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.INTERFACE_NAME_BRLAN0)
		    && !CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.INTERFACE_NAME_BRLAN1);
	    if (status) {
		LOGGER.info("STEP " + stepNum
			+ ": ACTUAL : Successfully validated VLAN bridge status is empty after OVS enabled ");
	    } else {
		LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 12 : VERIFY COLLECTING CPU AND MEMORY USAGE STATS FOR 5MINS WHEN OVS IS ENABLED
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the CPU and memory usage after enbling OVS";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify Collecting CPU and memory usage stats for 5mins when OVS is enabled.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command inside the RG console of the gateway for every one minute and collect the data for CPU and memory usage"
		    + "1. \"top -n 1 |grep -i Mem |sed  \"s/^[^0-9]*//;s/[^0-9].*$//\"\""
		    + "2. \"top -n 1 |grep CPU: |sed  \"s/^[^0-9]*//;s/[^0-9].*$//\"\""
		    + "3. Calculate the average for the data");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : CPU and memory usage should be collected");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("CPU and memory usage stats when OVS was enabled");
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		afterEnablingFeature = response;
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfuly collected CPU and memory usage details");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 13 : SET AND VERIFY ADVANCE SECURITY TO BE ENABLED
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to enable advance security";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Set and verify Advance Security to be enabled");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command 1.tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.Softflowd.Enable bool true"
		    + "2.tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_AdvancedSecurity.SafeBrowsing.Enable bool true");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Advance security must be enabled");
	    LOGGER.info("**********************************************************************************");
	    if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SOFTFLOWD, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE)) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_ADVANCED_SECURITY_SAFE_BROWSING,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully enabled Advaance security");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 14 : VERIFY THE FLOOD TRESHOLD TO TCP AND UPD PACKETS 
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to validate the expected flood threshold to tcp and upd packets";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify the flood treshold to tcp and upd packets ");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command- 1. cat /tcp_pkt_threshold"
		    + "2. cat /udp_pkt_threshold from the file path /proc/sys/net/flowmgr");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : The threshold must be greater than 10");
	    LOGGER.info("**********************************************************************************");
	    String tcpThreshold = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TCP_PACKET_THRESHOLD);
	    String udpThreshold = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_UDP_PACKET_THRESHOLD);
	    tcpThreshold = tcpThreshold.trim();
	    udpThreshold = udpThreshold.trim();
	    status = Integer.parseInt(tcpThreshold) >= BroadBandTestConstants.CONSTANT_10
		    && Integer.parseInt(udpThreshold) >= BroadBandTestConstants.CONSTANT_10;
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully validated the flood threshold value to be greater than 10 to tcp and upd packets");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 15 : UPDATE THE OVS DIABLE STATUS AS TRUE USING RFC
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to update the OVS disable using RFC";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Update the OVS diable status as true using RFC");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.OVS.Enable as false");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED :OVS enable status should be diabled using RFC");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.OVS_FEATURE_NAME, false);
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + ": ACTUAL : Sucessfully updated OVS disable status as true using RFC");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 16 : VERIFY THAT DEVICE LOGS THE TIME REQUIRED FOR COMPLETING THE INTERMEDIATE STAGES IN
	     * BOOTTIME.LOG
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    errorMessage = "Failed to capture the boot time logs ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify that device logs the time required for  completing  the intermediate stages in BootTime.log");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command in device console: cat /rdklogs/logs/BootTime.log to get the boot time values");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Device should log the time taken for completing intermediate stages");
	    LOGGER.info("**********************************************************************************");
	    HashMap<String, Integer> bootTimeBeforeOvs = new HashMap<String, Integer>();
	    try {
		for (String process : BroadBandCommandConstants.BOOTTIME_COMPONENT_LIST_OVS) {
		    String logsBeforeOvs = BroadBandCommonUtils.searchArmOrAtomLogFile(tapEnv, device, process,
			    BroadBandCommandConstants.FILE_BOOT_TIME_LOG);
		    status = CommonMethods.isNotNull(logsBeforeOvs);
		    if (status) {
			int uptimeBeforeOvs = Integer.parseInt(logsBeforeOvs.split("=")[1]);
			bootTimeBeforeOvs.put(process, uptimeBeforeOvs);
		    }
		}
		status = bootTimeBeforeOvs.size() == 8;
	    } catch (TestException e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully captured boot time logs");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 17 : COMPARE THE BOOT TIME LOG BEFORE OVS ENABLE AND AFTER ENABLED
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Observing boot time increase after Ovs enabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Compare the boot time log before OVS enable and after enabled");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the command in ARM console:  Boot time Ovs diasbled - Boot time Ovs enabled ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : There was not more than 5% increase in the boot time");
	    LOGGER.info("**********************************************************************************");
	    HashMap<String, Boolean> result = new HashMap<String, Boolean>();
	    for (String component : BroadBandCommandConstants.BOOTTIME_COMPONENT_LIST_OVS) {
		boolean output = false;
		LOGGER.info("bootTimeAfterOvs.get(component):"+bootTimeAfterOvs.get(component));
		LOGGER.info("bootTimeBeforeOvs.get(component):"+bootTimeBeforeOvs.get(component));
		int difference = bootTimeAfterOvs.get(component) - bootTimeBeforeOvs.get(component);
		LOGGER.info("Component:"+component+" Difference:"+difference);
		if (difference < BroadBandTestConstants.CONSTANT_10) {
		    output = true;
		}
		result.put(component, output);
	    }
	    int count = 0;
	    for (Map.Entry<String, Boolean> entry : result.entrySet()) {
		if (entry.getValue() == false) {
		    count++;
		    LOGGER.info("The value is more than 5% for " + entry.getKey());
		}
	    }
	    status = count == 0;
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified the boot time logs before OVS enable and after enabled");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 18 : VERIFY LOG MESSAGE FOR OVS DISABLED IN MESHAGENT LOG FILE
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = " Failed to get the log message for OVS disabled in MeshAgent log file ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION :Verify log message for OVS disabled in MeshAgent log file");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command in ATOM console: grep -i ovs /rdklogs/logs/MeshAgentLog.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Should get the log message for OVS disabled in MeshAgent log file");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_OVS, BroadBandCommandConstants.LOG_FILE_MESHAGENT,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTraceConstants.LOG_MESSAGE_OVS_DISABLED_MESH);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified log message for OVS disabled in MeshAgent log file");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 19 : VERIFY COLLECTING CPU AND MEMORY USAGE STATS FOR 5MINS WHEN OVS IS DISABLED
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to get the CPU and memory usage stats when OVS is disabled";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify Collecting CPU and memory usage stats for 5mins when OVS is disabled.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : a)Execute the following command inside the RG console of the gateway for every one minute and collect the data for CPU and memory usage, \\\"top -n 1 |grep -i Mem |sed  \\\"s/^[^0-9]*//;s/[^0-9].*$//\\\"\\\" and \\\"top -n 1 |grep CPU: |sed  \\\"s/^[^0-9]*//;s/[^0-9].*$//\\\"\\\"  b) Calculate the average for the data.\");");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : CPU and memory usage should be collected");
	    LOGGER.info("**********************************************************************************");
	    response = BroadBandWifiWhixUtils.calculateAverageCpuAndMemoryUtilisation(device, tapEnv,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("CPU and memory usage stats when OVS was disabled");
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		beforeEnablingFeature = response;
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfuly collected CPU and memory usage details");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 20 : VERIFY NEGATIVE IMPACT OF ENABLING OVS ON CPU AND MEMORY
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "There is a negative impact on CPU & Memory after enabling OVS";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify negative impact of enabling OVS on CPU and Memory");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute 1. Log in web Interface with User Id and Password."
		    + "2. Compare the averages calculated for CPU utilisation and memory utilisation in step 2 & 12");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : The difference in average should be within 10%, indicating that the feature doesn't have any negative impact on the device ");
	    LOGGER.info("**********************************************************************************");
	    BroadBandResultObject bandResultObject = null;
	    bandResultObject = BroadBandWifiWhixUtils
		    .validateCpuAndMemoryUtilisationForNegativeEffect(beforeEnablingFeature, afterEnablingFeature);
	    status = bandResultObject.isStatus();
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified that there is no negative impact of enabling OVS on CPU and memory");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 21 : VERIFY VLAN BRIDGES WHEN OVS WAS DISABLED UNDER OVS BRIDGE CONTROL
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to validate the VLAN bridge status";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify VLAN bridges when OVS was disabled under OVS bridge control");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command: ovs-vsctl show");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : VLAN Bridges should be empty");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.OVS_SHOW_COMMAND);
	    status = CommonMethods.isNotNull(response) && !response.contains(BroadBandTestConstants.STRING_PORT_BRLAN0)
		    && !response.contains(BroadBandTestConstants.STRING_BRIDGE_BRLAN0);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfuly validated VLAN bridge status");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 22 : VERIFY BRIDGE AND VLAN COMMAND INTERFACE UNDER LINUX BRIDGE CONTROL
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = " Failed to validate the VLAN bridge status under linux bridge control ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify bridge and VLAN command interface under linux bridge control");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command: brctl show");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Bridge and VLAN details should be retrieved");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.BRCTL_SHOW);
	    status = CommonMethods.isNotNull(response)
		    && (response.contains(BroadBandTestConstants.INTERFACE_NAME_BRLAN0)
			    || response.contains(BroadBandTestConstants.INTERFACE_NAME_BRLAN1));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Sucessfully validated VLAN bridge status under linux bridge control");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1: DESCRIPTION: Remove rfc.properties file from /nvram directory");
	    LOGGER.info("POST-CONDITION 1: ACTION: Execute command: rm /nvram/rfc.properties");
	    LOGGER.info("POST-CONDITION 1: EXPECTED: Should be deleted rfc.properties file");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Failed to remove rfc.properties file from /nvram directory";
	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES);
	    if (status) {
		LOGGER.info(
			"POST-CONDITION 1: ACTUAL :Successfully removed the rfc.properties file in /nvram directory");
	    } else {
		LOGGER.error("POST-CONDITION 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS: FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE:TC-RDKB-OVS-1002");
   }
}
