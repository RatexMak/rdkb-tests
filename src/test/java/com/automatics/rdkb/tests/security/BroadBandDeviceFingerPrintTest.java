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

/**
 * Test class with test case related to Security
 * 
 * @refactor Athira
 *
 */
import java.util.HashMap;
import java.util.Map;

import org.testng.annotations.Test;
import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
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

}
