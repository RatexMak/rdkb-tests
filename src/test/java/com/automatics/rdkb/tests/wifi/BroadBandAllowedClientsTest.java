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

package com.automatics.rdkb.tests.wifi;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.enums.ProtocolOperationTypeEnum;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.utils.CommonMethods;
import com.automatics.exceptions.TestException;
import com.automatics.providers.tr69.Parameter;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.enums.TR69ParamDataType;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.providers.tr69.Parameter;
import com.automatics.rdkb.utils.tr69.Tr69RestAPIUtils;

public class BroadBandAllowedClientsTest extends AutomaticsTestBase {

	/**
	 * Test to verify the default number of allowed clients for PublicWiFi Secure
	 * and open SSID using SNMP, WebPA.
	 * 
	 * 
	 * <ol>
	 * <li>Pre Condition: Factory reset the device to check default values</li>
	 * <li>STEP 1: Verify the default number of allowed clients for 2.4 GHz
	 * PublicWiFi Secure SSID using WebPA</li>
	 * <li>STEP 2: Verify the default number of allowed clients for 5 GHz PublicWiFi
	 * Secure SSID using WebPA</li>
	 * <li>STEP 3: Verify the default number of allowed clients for 2.4 GHz open
	 * SSID using WebPA</li>
	 * <li>STEP 4: Verify the default number of allowed clients for 5 GHz open SSID
	 * using WebPA</li>
	 * <li>Post Condition: Reactivate the device, if factory reset is performed</li>
	 * </ol>
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Sathya Kishore
	 * @refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-5020")
	public void testToVerifyDefaultValueOfNumberOfAllowedClientsUsingSnmp(Dut device) {

		String testCaseId = "TC-RDKB-WIFI-520";
		String stepNumber = "s1";
		int step = 0;
		boolean status = false; // stores the test status
		String errorMessage = null; // stores the error message
		boolean isFactoryReset = false; // stores the status of factory reset
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5020");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the default number of allowed clients for PublicWiFi Secure and open SSID using SNMP, WebPA");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION :Factory reset the device to check default values");
		LOGGER.info(" 1: Verify the default number of allowed clients for 2.4 GHz PublicWiFi Secure SSID using WebPA");
		LOGGER.info(" 2: Verify the default number of allowed clients for 5 GHz PublicWiFi Secure SSID using WebPA");
		LOGGER.info(" 3: Verify the default number of allowed clients for 2.4 GHz open SSID using WebPA");
		LOGGER.info(" 4: Verify the default number of allowed clients for 5 GHz open SSID using WebPA");
		LOGGER.info("#######################################################################################");
		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("#####################################################################################");
			LOGGER.info("PRE-CONDITION : DESCRIPTION : Factory reset the device to check default values");
			LOGGER.info("PRE-CONDITION : ACTION : Perform factory reset using Webpa");
			LOGGER.info("PRE-CONDITION : EXPECTED : Device is factory resetted successfully");
			LOGGER.info("#####################################################################################");
			status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

			if (status) {
				LOGGER.info("PRE-CONDITION : ACTUAL : Factory Reset is successful");
				isFactoryReset = true;
				LOGGER.info("Waiting till uptime is 6 mins.");
				int count = 0;
				do {
					status = BroadBandCommonUtils.getUptimeAfterReboot(device, tapEnv) < 6;
					LOGGER.info("Uptime of device: " + BroadBandCommonUtils.getUptimeAfterReboot(device, tapEnv));
					count++;
				} while (status && count <= 10
						&& CommonMethods.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			} else {
				LOGGER.error("PRE-CONDITION : ACTUAL : Factory Reset is not successful");
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + "Factory Resetting the device failed");
			}
			LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

			String webPaOutput = null; // stores WebPA output

			/**
			 * Step 1: Verify the default number of allowed clients for 2.4 GHz PublicWiFi
			 * Secure SSID using SNMP
			 * 
			 */
			step++;
			stepNumber = "S" + step;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + step
					+ ":DESCRIPTION: Verify the default number of allowed clients for 2.4 GHz PublicWiFi Secure SSID using WebPA");
			LOGGER.info("STEP " + step
					+ ":ACTION:Execute the command to verify the default number of allowed clients for 2.4 GHz PublicWiFi Secure SSID using WebPA parameter Device.WiFi.AccessPoint.10005.X_CISCO_COM_BssMaxNumSta");
			LOGGER.info("STEP " + step
					+ ":EXPECTED: The returned value should be 5 for Residential Gateways and 15 for Commercial Gateways");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Invalid/Null value retrieved for default number of allowed clients for 2.4 GHz PublicWiFi Secure SSID via WebPA";
			webPaOutput = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_2_4);
			status = CommonMethods.isNotNull(webPaOutput)
					&& BroadBandWiFiUtils.validateAllowedNoOfClientsValueRetrievedFromWebPA(device, webPaOutput);
			if (status) {
				LOGGER.info("STEP " + step
						+ ": ACTUAL : Default value for number of allowed clients for 2.4 GHz PublicWiFi Secure SSID has been retrieved using WebPA and verified successfully");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 2: Verify the default number of allowed clients for 5 GHz PublicWiFi
			 * Secure SSID using WebPA
			 * 
			 */
			step++;
			stepNumber = "S" + step;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + step
					+ ":DESCRIPTION: Verify the default number of allowed clients for 5 GHz PublicWiFi Secure SSID using WebPA");
			LOGGER.info("STEP " + step
					+ ":ACTION:Execute the command Execute the command to verify the default number of allowed clients for 5GHz PublicWiFi Secure SSID using WebPA parameter Device.WiFi.AccessPoint.10105.X_CISCO_COM_BssMaxNumSta");
			LOGGER.info("STEP " + step
					+ ":EXPECTED: The returned value should be 5 for Residential Gateways and 15 for Commercial Gateways");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Invalid/Null value retrieved for default number of allowed clients for 5 GHz PublicWiFi Secure SSID via WebPA";
			webPaOutput = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_5);
			status = CommonMethods.isNotNull(webPaOutput)
					&& BroadBandWiFiUtils.validateAllowedNoOfClientsValueRetrievedFromWebPA(device, webPaOutput);
			if (status) {
				LOGGER.info("STEP " + step
						+ ": ACTUAL : Default value for number of allowed clients for 5 GHz PublicWiFi Secure SSID has been retrieved using WebPA and verified successfully");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 3: Verify the default number of allowed clients for 2.4 GHz open SSID
			 * using WebPA
			 * 
			 */
			step++;
			stepNumber = "S" + step;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + step
					+ ": DESCRIPTION: Verify the default number of allowed clients for 2.4 GHz open SSID using WebPA");
			LOGGER.info("STEP " + step
					+ ":ACTION: Execute the command to verify the default number of allowed clients for 2.4 GHz open SSID using WebPA parameter Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta");
			LOGGER.info("STEP " + step
					+ ":EXPECTED: The returned value should be 5 for Residential Gateways and 15 for Commercial Gateways");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Invalid/Null value retrieved for default number of allowed clients for 2.4 GHz Open SSID via WebPA";

			webPaOutput = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
			status = CommonMethods.isNotNull(webPaOutput)
					&& BroadBandWiFiUtils.validateAllowedNoOfClientsValueRetrievedFromWebPA(device, webPaOutput);
			if (status) {
				LOGGER.info("STEP " + step
						+ ":ACTUAL : Default value for number of allowed clients for 2.4 GHz Open SSID has been retrieved using WebPA and verified successfully");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

			/**
			 * Step 4: Verify the default number of allowed clients for 5 GHz open SSID
			 * using WebPA
			 * 
			 */
			step++;
			stepNumber = "S" + step;
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + step
					+ ":DESCRIPTION: Verify the default number of allowed clients for 5 GHz open SSID using WebPA");
			LOGGER.info("STEP " + step
					+ ":ACTION: Execute the command to verify the default number of allowed clients for 5 GHz open SSID using WebPA parameter Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta");
			LOGGER.info("STEP " + step
					+ ": EXPECTED: The returned value should be 5 for Residential Gateways and 15 for Commercial Gateways");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Invalid/Null value retrieved for default number of allowed clients for 5 GHz Open SSID via WebPA";
			webPaOutput = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			status = CommonMethods.isNotNull(webPaOutput)
					&& BroadBandWiFiUtils.validateAllowedNoOfClientsValueRetrievedFromWebPA(device, webPaOutput);
			if (status) {
				LOGGER.info("STEP " + step
						+ ": ACTUAL : Default value for number of allowed clients for 5 GHz Open SSID has been retrieved using WebPA and verified successfully");
			} else {
				LOGGER.error("STEP " + step + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		} catch (Exception testException) {
			errorMessage = "Exception occurred while trying to verify the default number of allowed clients for PublicWiFi Secure and open SSID : "
					+ testException.getMessage();
			LOGGER.error(errorMessage);
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			if (isFactoryReset) {
				status = false;				
				BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
						BroadBandTestConstants.CONSTANT_1);				
			} else {
				BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			}
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
	}
	
    /**
     * Test to verify number of allowed clients for Public Secure and open SSID can be writable using WEBPA/TR-69 and
     * changes are logged in WEBPA/TR-69 log file
     * 
     * <ol>
     * <li>STEP 1: Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using WebPA
     * and changes are logged in WEBPA log file</li>
     * <li>STEP 2: Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using WebPA
     * and changes are logged in WEBPA log file</li>
     * <li>STEP 3: Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using WebPA and changes
     * are logged in WEBPA log file</li>
     * <li>STEP 4: Verify the number of allowed clients for 5 GHz Open SSID can be writable using WebPA and changes are
     * logged in WEBPA log file</li>
     * <li>STEP 5: Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using TR-069
     * and changes are logged in TR-069 log file</li>
     * <li>STEP 6: Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using TR-069
     * and changes are logged in TR-069 log file</li>
     * <li>STEP 7: Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using TR-069 and changes
     * are logged in TR-069 log file</li>
     * <li>STEP 8: Verify the number of allowed clients for 5 GHz Open SSID can be writable using TR-069 and changes
     * are logged in TR-069 log file</li> *
     * <li>Post Condition: Factory Reset & Reactivate the device</li>
     * </ol>
     * 
     * @param device
     *            The device to be used.
	 * @Author Sathya Kishore
	 * @refactor Athira
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-5021")
    public void verifyNumberOfAllowedClientsCanBeWritableUsingSnmp(Dut device) {
	String testCaseId = "TC-RDKB-WIFI-521";
	String stepNumber = "s1";
	int step=0;
	boolean status = false; // stores the test status
	String errorMessage = null; // stores the error message
	boolean isAtom = false;
	String response = null; // stores TR-069 Set execution status

	String deviceDateTime = null;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-5021");
	    LOGGER.info(
		    "TEST DESCRIPTION: verify number of allowed clients for Public Secure and open SSID can be writable using SNMP and and changes are logged in SNMP log file");
	    LOGGER.info("TEST STEPS : ");
	   
	    LOGGER.info(
		    "1. Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using WebPA and changes are logged in WEBPA log file ");
	    LOGGER.info(
		    "2. Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using WebPA and changes are logged in WEBPA log file");
	    LOGGER.info(
		    "3. Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using WebPA and changes are logged in WEBPA log file ");
	    LOGGER.info(
		    "4. erify the number of allowed clients for 5 GHz Open SSID can be writable using WebPA and changes are logged in WEBPA log file");
	    LOGGER.info(
		    "5. Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using TR-069 and changes are logged in TR-069 log file ");
	    LOGGER.info(
		    "6. Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using TR-069 and changes are logged in TR-069 log file ");
	    LOGGER.info(
		    "7. Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using TR-069 and changes are logged in TR-069 log file ");
	    LOGGER.info(
		    "8. Verify the number of allowed clients for 5 GHz Open SSID can be writable using TR-069 and changes are logged in TR-069 log file");
	    LOGGER.info("#######################################################################################");

	    
	    // Checking atom sync to verify WEBPA logs in atom console for Atom based devices
	    isAtom = CommonMethods.isAtomSyncAvailable(device, tapEnv);

	    /**
	     * Step 1: Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using
	     * WebPA and changes are logged in WEBPA log file
	     *
	     */
	    step++;
	    stepNumber = "S" + step;
	    status=false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP"+step+" : DESCRIPTION : Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using WebPA and changes are logged in WEBPA log file");
	    LOGGER.info("STEP"+step+" : ACTION : Execute the command : "
		    + BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_2_4);
	    LOGGER.info(
		    "STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in WebPA log file");
	    LOGGER.info("**********************************************************************************");
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_2_4,
		    AutomaticsConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_FOUR);
	    errorMessage = "Number of allowed clients for 2.4 GHz Public Secure SSID can not be writable using WebPA";
	    if (status) {
		status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_2_4,
				BroadBandTestConstants.STRING_VALUE_FOUR, BroadBandCommandConstants.LOG_FILE_WEBPA_TEXT,
				ProtocolOperationTypeEnum.WEBPA_SET, isAtom),
			isAtom);
		errorMessage = "Change to number of allowed clients for 2.4 GHz Public Secure SSID using WebPA has not been logged in WebPA log file";
	    }
	    if (status) {
		LOGGER.info(
			"STEP"+step+" : ACTUAL: Number of allowed clients for 2.4 GHz Public Secure SSID can be writable & the activity is logged in WebPA log file");
	    } else {
		LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * Step 2: Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using
	     * WebPA and changes are logged in WEBPA log file
	     * 
	     */
	    step++;
	    stepNumber = "S" + step;
	    status=false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP"+step+"  : DESCRIPTION : Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using WebPA and changes are logged in WEBPA log file");
	    LOGGER.info("STEP"+step+" : ACTION : Execute webpa command : "
		    + BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_5);
	    LOGGER.info(
		    "STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in WebPA log file");
	    LOGGER.info("**********************************************************************************");
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_5, AutomaticsConstants.CONSTANT_1,
		    BroadBandTestConstants.STRING_VALUE_FOUR);
	    errorMessage = "Number of allowed clients for 5 GHz Public Secure SSID can not be writable using WebPA";
	    if (status) {
		status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_5,
				BroadBandTestConstants.STRING_VALUE_FOUR, BroadBandCommandConstants.LOG_FILE_WEBPA_TEXT,
				ProtocolOperationTypeEnum.WEBPA_SET, isAtom),
			isAtom);
		errorMessage = "Change to number of allowed clients for 5 GHz Public Secure SSID using WebPA has not been logged in WebPA log file";
	    }
	    if (status) {
		LOGGER.info(
			"STEP"+step+" : ACTUAL:Number of allowed clients for 5 GHz Public Secure SSID can be writable & the activity is logged in WebPA log file");
	    } else {
		LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	    /**
	     * Step 3: Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using WebPA and
	     * changes are logged in WEBPA log file
	     * 
	     */
	    step++;
	    stepNumber = "S" + step;
	    status=false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP"+step+"  : DESCRIPTION : Verify the number of allowed clients for 2.4 GHz Open SSID  can be writable using WebPA and changes are logged in WEBPA log file");
	    LOGGER.info("STEP"+step+" : ACTION : Execute webpa command "
		    + BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
	    LOGGER.info(
		    "STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in WebPA log file");
	    LOGGER.info("**********************************************************************************");
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
	    LOGGER.info("STEP"+step+" : deviceDateTime " +deviceDateTime);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4, AutomaticsConstants.CONSTANT_1,
		    BroadBandTestConstants.STRING_VALUE_FOUR);
	    errorMessage = "Number of allowed clients for 2.4 GHz Open SSID can not be writable using WebPA";
	    if (status) {
		status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
				BroadBandTestConstants.STRING_VALUE_FOUR, BroadBandCommandConstants.LOG_FILE_WEBPA_TEXT,
				ProtocolOperationTypeEnum.WEBPA_SET, isAtom),
			isAtom);
		errorMessage = "Change to number of allowed clients for 2.4 GHz Open SSID using WebPA has not been logged in WebPA log file";
	    }
	    if (status) {
		LOGGER.info(
			"STEP"+step+" : ACTUAL:Number of allowed clients for 2.4 GHz Open SSID can be writable & the activity is logged in WebPA log file");
	    } else {
		LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    /**
	     * Step 4: Verify the number of allowed clients for 5 GHz Open SSID can be writable using WebPA and changes
	     * are logged in WEBPA log file
	     * 
	     */
	    step++;
	    stepNumber = "S" + step;
	    status=false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP"+step+"  : DESCRIPTION : Verify the number of allowed clients for  5 GHz Open SSID  can be writable using WebPA and changes are logged in WEBPA log file");
	    LOGGER.info("STEP"+step+" : ACTION : Execute webpa command "
		    + BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
	    LOGGER.info(
		    "STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in WebPA log file");
	    LOGGER.info("**********************************************************************************");
	    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv, device);
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5, AutomaticsConstants.CONSTANT_1,
		    BroadBandTestConstants.STRING_VALUE_FOUR);
	    errorMessage = "Number of allowed clients for 5 GHz Open SSID can not be writable using WebPA";
	    if (status) {
		status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
				BroadBandTestConstants.STRING_VALUE_FOUR, BroadBandCommandConstants.LOG_FILE_WEBPA_TEXT,
				ProtocolOperationTypeEnum.WEBPA_SET, isAtom),
			isAtom);
		errorMessage = "Change to number of allowed clients for 5 GHz Open SSID using WebPA has not been logged in WebPA log file";
	    }
	    if (status) {
		LOGGER.info(
			"STEP"+step+" : ACTUAL:Number of allowed clients for 5 GHz Open SSID can be writable & the activity is logged in WebPA log file");
	    } else {
		LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    // updating isAtom to false since TR-069 logs needs to be checked in ARM for
	    // both Atom based and Arm based devices
	    isAtom = false;

	    // Setting to TR69/ACS execution
	    LOGGER.info("Setting isWebPA - false");
	    System.setProperty(BroadBandTestConstants.SYSTEM_PROPERTY_ISWEBPA, BroadBandTestConstants.FALSE);

	    boolean isSyndicatePartner = BroadBandCommonUtils.verifySyndicatePartnerIdOnDevice(device, tapEnv);
	    if (!isSyndicatePartner || DeviceModeHandler.isDSLDevice(device) || CommonMethods.isRunningEthwanMode()) {
		step = 4;
		while (step < 8) {
		    step++;
		    stepNumber = "s" + step;
		    LOGGER.error("STEP " + step + ": ACTUAL : Test step not applicable for " + device.getModel());
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    } else {
		/**
		 * Step 5: Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable
		 * using TR-069 and changes are logged in TR-069 log file
		 *
		 */
		step++;
		    stepNumber = "S" + step;
		    status=false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP"+step+"  : DESCRIPTION : Verify the number of allowed clients for 2.4 GHz Public Secure SSID can be writable using TR-069 and changes are logged in TR-069 log file");
		LOGGER.info(
			"STEP"+step+" : ACTION : Set the TR-181 parameter 'Device.WiFi.AccessPoint.10005.X_CISCO_COM_BssMaxNumSta' with value '11' and data type as 'int' via TR-69/ACS and check the logs under /rdklogs/logs/TR69log.txt.0");
		LOGGER.info(
			"STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in TR-069 log file");
		LOGGER.info("**********************************************************************************");
		try {
			Parameter setParam = new Parameter();
		    
		    setParam.setDataType(TR69ParamDataType.INT.get());
		    setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_2_4);
		    setParam.setParamValue(BroadBandTestConstants.STRING_VALUE_SIX);
		    List<Parameter> parameters = new ArrayList<Parameter>();
		    parameters.add(setParam);
		    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv,
			    device);
		    LOGGER.info("deviceDateTime " + deviceDateTime);
		    long startTime = System.currentTimeMillis();
		    LOGGER.info("startTime " + startTime);
		    response = tapEnv.setTR69ParameterValues(device, parameters);
		    LOGGER.info("response " + response);
		} catch (Exception exception) {
		    // Log & Suppress the Exception.
		    LOGGER.error("EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: "
			    + exception.getMessage());
		}
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
		errorMessage = "Number of allowed clients for 2.4 GHz Public Secure SSID can not be writable using TR-069";
		if (status) {
		    status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			    BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_2_4,
				    BroadBandTestConstants.STRING_VALUE_SIX,
				    BroadBandCommandConstants.LOG_FILE_TR69_TEXT, ProtocolOperationTypeEnum.TR69_SET,
				    isAtom),
			    isAtom);
		    errorMessage = "Change to number of allowed clients for 2.4 GHz Public Secure SSID using TR-069 has not been logged in TR-069 log file";
		}
		if (status) {
		    LOGGER.info(
			    "STEP"+step+" : ACTUAL:Number of allowed clients for 2.4 GHz Public Secure SSID can be writable & the activity is logged in TR-69 log file");
		} else {
		    LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		/**
		 * Step 6: Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using
		 * TR-069 and changes are logged in TR-069 log file
		 *
		 */
		step++;
		    stepNumber = "S" + step;
		    status=false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP"+step+"  : DESCRIPTION : Verify the number of allowed clients for 5 GHz Public Secure SSID can be writable using TR-069 and changes are logged in TR-069 log file");
		LOGGER.info(
			"STEP"+step+" : ACTION : Set the TR-181 parameter 'Device.WiFi.AccessPoint.10105.X_CISCO_COM_BssMaxNumSta' with value '11' and data type as 'int'  via TR-69/ACS and check the logs under /rdklogs/logs/TR69log.txt.0");
		LOGGER.info(
			"STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in TR-069 log file");
		LOGGER.info("**********************************************************************************");
		try {
			Parameter setParam = new Parameter();

		    setParam.setDataType(TR69ParamDataType.INT.get());
		    setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_5);
		    setParam.setParamValue(BroadBandTestConstants.STRING_VALUE_SIX);
		    List<Parameter> parameters = new ArrayList<Parameter>();
		    parameters.add(setParam);
		    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv,
			    device);
		    response = tapEnv.setTR69ParameterValues(device, parameters);
		} catch (Exception exception) {
		    // Log & Suppress the Exception.
		    LOGGER.error("EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: "
			    + exception.getMessage());
		}
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
		errorMessage = "Number of allowed clients for 5 GHz Public Secure SSID can not be writable using TR-069";
		if (status) {
		    status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			    BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_SECURE_SSID_5,
				    BroadBandTestConstants.STRING_VALUE_SIX,
				    BroadBandCommandConstants.LOG_FILE_TR69_TEXT, ProtocolOperationTypeEnum.TR69_SET,
				    isAtom),
			    isAtom);
		    errorMessage = "Change to number of allowed clients for 5 GHz Public Secure SSID using TR-069 has not been logged in TR-069 log file";
		}
		if (status) {
		    LOGGER.info(
			    "STEP"+step+" : ACTUAL:Number of allowed clients for 5 GHz Public Secure SSID can be writable & the activity is logged in TR-69 log file");
		} else {
		    LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		/**
		 * Step 7: Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using TR-069 and
		 * changes are logged in TR-069 log file
		 *
		 */
		step++;
		    stepNumber = "S" + step;
		    status=false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP"+step+"  : DESCRIPTION : Verify the number of allowed clients for 2.4 GHz Open SSID can be writable using TR-069 and changes are logged in TR-069 log file");
		LOGGER.info(
			"STEP"+step+" : ACTION : Set the TR-181 parameter 'Device.WiFi.AccessPoint.10003.X_CISCO_COM_BssMaxNumSta' with value '11' and data type as 'int' via TR-69/ACS and check the logs under /rdklogs/logs/TR69log.txt.0");
		LOGGER.info(
			"STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in TR-069 log file");
		LOGGER.info("**********************************************************************************");
		try {
			Parameter setParam = new Parameter();
			
		    setParam.setDataType(TR69ParamDataType.INT.get());
		    setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4);
		    setParam.setParamValue(BroadBandTestConstants.STRING_VALUE_SIX);
		    List<Parameter> parameters = new ArrayList<Parameter>();
		    parameters.add(setParam);
		    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv,
			    device);
		    response = tapEnv.setTR69ParameterValues(device, parameters);
		} catch (Exception exception) {
		    // Log & Suppress the Exception.
		    LOGGER.error("EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: "
			    + exception.getMessage());
		}
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
		errorMessage = "Number of allowed clients for 2.4 GHz Open Secure SSID can not be writable using TR-069";
		if (status) {
		    status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			    BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_2_4,
				    BroadBandTestConstants.STRING_VALUE_SIX,
				    BroadBandCommandConstants.LOG_FILE_TR69_TEXT, ProtocolOperationTypeEnum.TR69_SET,
				    isAtom),
			    isAtom);
		    errorMessage = "Change to number of allowed clients for 2.4 GHz Open Secure SSID using TR-069 has not been logged in TR-069 log file";
		}
		if (status) {
		    LOGGER.info(
			    "STEP"+step+" : ACTUAL:Number of allowed clients for 2.4 GHz Open SSID can be writable & the activity is logged in TR-69 log file");
		} else {
		    LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

		/**
		 * Step 8: Verify the number of allowed clients for 5 GHz Open SSID can be writable using TR-069 and
		 * changes are logged in TR-069 log file
		 *
		 */
		step++;
		    stepNumber = "S" + step;
		    status=false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP"+step+" : DESCRIPTION : Verify the number of allowed clients for 5 GHz Open SSID can be writable using TR-069 and changes are logged in TR-069 log file");
		LOGGER.info(
			"STEP"+step+" : ACTION : Set the TR-181 parameter 'Device.WiFi.AccessPoint.10103.X_CISCO_COM_BssMaxNumSta' with value '11' and data type as 'int' via TR-69/ACS and check the logs under /rdklogs/logs/TR69log.txt.0");
		LOGGER.info(
			"STEP"+step+" : EXPECTED : Set should be successful and the activity should be logged in TR-069 log file");
		LOGGER.info("**********************************************************************************");
		try {
			Parameter setParam = new Parameter();
		    setParam.setDataType(TR69ParamDataType.INT.get());
		    setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5);
			setParam.setParamValue(BroadBandTestConstants.STRING_VALUE_SIX);
			List<Parameter> parameters = new ArrayList<Parameter>();
		    parameters.add(setParam);
		    deviceDateTime = BroadBandCommonUtils.getCurrentTimeStampOnDeviceFromAtomorArmConsole(tapEnv,
			    device);
		    response = tapEnv.setTR69ParameterValues(device, parameters);
		} catch (Exception exception) {
		    // Log & Suppress the Exception.
		    LOGGER.error("EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: "
			    + exception.getMessage());
		}
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
		errorMessage = "Number of allowed clients for 5 GHz Open Secure SSID can not be writable using TR-069";
		if (status) {
		    status = BroadBandWiFiUtils.grepLogAndVerifyUsingTimeStamp(tapEnv, device, deviceDateTime,
			    BroadBandWiFiUtils.generateGrepForProtocolOperationType(
				    BroadBandWebPaConstants.WEBPA_PARAM_ALLOWED_CLIENT_LIMIT_OPEN_SSID_5,
				    BroadBandTestConstants.STRING_VALUE_SIX,
				    BroadBandCommandConstants.LOG_FILE_TR69_TEXT, ProtocolOperationTypeEnum.TR69_SET,
				    isAtom),
			    isAtom);
		    errorMessage = "Change to number of allowed clients for 5 GHz Open Secure SSID using TR-069 has not been logged in TR-069 log file";
		}
		if (status) {
		    LOGGER.info(
			    "STEP"+step+" : ACTUAL:Number of allowed clients for 5 GHz Open SSID can be writable & the activity is logged in TR-69 log file");
		} else {
		    LOGGER.error("STEP"+step+" : ACTUAL: " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    }
	} catch (Exception testException) {
	    errorMessage = "Exception occurred while trying to verify number of allowed clients for Public Secure and open SSID can be writable using SNMP"
		    + testException.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	} finally {
	    // Setting back to Webpa execution
	    LOGGER.info("Setting isWebPA - true");
	    System.setProperty(BroadBandTestConstants.SYSTEM_PROPERTY_ISWEBPA, BroadBandTestConstants.TRUE);
	    LOGGER.info("### POST-CONDITION ### BEGIN FACTORY RESET AND DEVICE REACTIVATION");
	    if (BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device)) {
		BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
	    } else {
		LOGGER.info("FACTORY RESET FAILED");
	    }
	    LOGGER.info("### POST-CONDITION ### END FACTORY RESET AND DEVICE REACTIVATION");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-5021");
    LOGGER.info("#######################################################################################");
    
    }
}
