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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;

/**
 * Test class with test case related to Default SSID validation
 * 
 * @author Praveenkumar Paneerselvam
 * @refactor Govardhan
 *
 */
public class BroadBandDefaultSSIDTest extends AutomaticsTestBase {

	/**
	 *
	 * Test Case # 1: Verify default SSID using WebPA.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <li>S1) Verify retrieving default SSID for 2.4 Ghz using WebPA get
	 * request.</li>
	 * <li>S2) Verify retrieving default SSID for 5 Ghz using WebPA get
	 * request.</li>
	 * <li>S3) Modify Current SSID name of 2.4 Ghz and 5 Ghz.</li>
	 * <li>S4) Perform Factory reset on the device using WebPA request..</li>
	 * <li>S5) Verify the Current SSID value for 2.4 Ghz using WebPA request.</li>
	 * <li>S6) Verify the Current SSID value for 5 Ghz using WebPA request.</li>
	 * </ol>
	 * 
	 * @author Praveenkumar Paneerselvam
	 * @refactor Govardhan
	 * @param device {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-1010")
	public void testDefaultSSIDUsingWebPA(Dut device) {
		String testCaseId = "TC-RDKB-WIFI-010";
		String stepNumber = "s1";
		boolean status = false;
		String errorMessage = null;
		String defaultSSIDName2dot4Ghz = null;
		String currentSSIDName2dot4Ghz = null;
		boolean factoryResetStatus = false;
		String defaultSSIDName5Ghz = null;
		String currentSSIDName5Ghz = null;
		LOGGER.info("###################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-1010");
		LOGGER.info("TEST DESCRIPTION: Verify default SSID using WebPA");

		LOGGER.info("TEST STEPS : ");

		LOGGER.info("1. Verify retrieving default SSID for 2.4 Ghz using WebPA get request.  ");
		LOGGER.info("2. Verify retrieving default SSID for 5 Ghz using WebPA get request");
		LOGGER.info("3. Modify Current SSID name of 2.4 Ghz and 5 Ghz if current SSID is default SSID");
		LOGGER.info("4. Perform Factory reset on the device using WebPA request ");
		LOGGER.info("5. Verify the Current SSID value for 2.4 Ghz using WebPA request ");
		LOGGER.info("6. Verify the Current SSID value for 5 Ghz using WebPA request");
		LOGGER.info("############################################################################");

		try {

			/**
			 * Step 1 : Verify retrieving default SSID for 2.4 Ghz using WebPA get request.
			 */
			stepNumber = "s1";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify retrieving default SSID for 2.4 Ghz using WebPA get request");
			LOGGER.info("STEP 1 : ACTION : Execute webpa command:"
					+ BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ);
			LOGGER.info(
					"STEP 1 : EXPTECTED : WebPA request must return success message with default SSID value for 2.4Ghz.");
			LOGGER.info("**********************************************************************************");
			defaultSSIDName2dot4Ghz = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ);
			LOGGER.info("Default SSID for 2.4 Ghz of the device is " + defaultSSIDName2dot4Ghz);
			status = CommonMethods.isNotNull(defaultSSIDName2dot4Ghz);
			errorMessage = "Failed to get default SSID using WebPa parameter "
					+ BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : WebPA request returned success message with default SSID value as "
						+ defaultSSIDName2dot4Ghz + " for 2.4Ghz.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 2 : Verify retrieving default SSID for 5 Ghz using WebPA get request.
			 */
			stepNumber = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Verify retrieving default SSID for 5 Ghz using WebPA get request");
			LOGGER.info(
					"STEP 2 : ACTION : Execute webpa command:" + BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ);
			LOGGER.info(
					"STEP 2 : EXPTECTED : WebPA request must return success message with default SSID value for 5Ghz.");
			LOGGER.info("**********************************************************************************");
			defaultSSIDName5Ghz = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ);
			LOGGER.info("Default SSID for 5 Ghz of the device is " + defaultSSIDName5Ghz);
			status = CommonMethods.isNotNull(defaultSSIDName5Ghz);
			errorMessage = "Failed to get default SSID using WebPa parameter "
					+ BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ;
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : WebPA request returned success message with default SSID value as "
						+ defaultSSIDName5Ghz + " for 5Ghz.");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 3 : Modify Current SSID name of 2.4 Ghz and 5 Ghz.
			 */

			stepNumber = "s3";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Modify Current SSID name of 2.4 Ghz and 5 Ghz if current SSID is default SSID");
			LOGGER.info("STEP 3 : ACTION : Execute webpa command to Modify Current SSID name of 2.4 Ghz and 5 Ghz if current SSID is default SSID");
			LOGGER.info(
					"STEP 3 : EXPTECTED : Current SSID name for 2.4 Ghz and 5 Ghz should be a value other than default SSID.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to set 2.4 Ghz SSID name using WebPA parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
			
			currentSSIDName2dot4Ghz = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			LOGGER.info("SSID for 2.4 Ghz of the device is " + currentSSIDName2dot4Ghz);
			
			// Set Current 2.4 Ghz SSID name, other than default value SSID
			
			if (currentSSIDName2dot4Ghz.equals(defaultSSIDName2dot4Ghz))
			{
				String newSSIDName2Dot4Ghz = BroadBandTestConstants.STRING_TEST_1;
				LOGGER.info("Modify Current SSID name of 2.4 Ghz, other than default SSID value");
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
						BroadBandTestConstants.CONSTANT_0, newSSIDName2Dot4Ghz);
				LOGGER.info("Is Current SSID for 2.4 Ghz modified - " + status);
				if (status) {
					errorMessage = "Failed to set 5 Ghz SSID name using WebPA parameter "
							+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
					// Set Current 5Ghz SSID name, other than default value SSID
					currentSSIDName5Ghz = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
					LOGGER.info("SSID for 5 Ghz of the device is " + currentSSIDName5Ghz);
					if (currentSSIDName5Ghz.equals(defaultSSIDName5Ghz))
					{
						String newSSIDName5Ghz = BroadBandTestConstants.STRING_TEST_1;
						LOGGER.info("Modify Current SSID name of 2.4 Ghz, other than default SSID value");
						status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
								BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
								BroadBandTestConstants.CONSTANT_0, newSSIDName5Ghz);
						LOGGER.info("Is Current SSID for 5 Ghz modified - " + status);						
					}
					else
					{
						status = true;					
					}
					
				}
			}
			else
			{
				status = true;
			}

			if (status) {
				LOGGER.info("STEP 3: ACTUAL : SSID name of 2.4 Ghz and 5 Ghz are not default SSID name");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 4 : Perform Factory reset on the device using WebPA request.
			 */
			stepNumber = "s4";
			status = false;
			errorMessage = "Failed to factory reset wifi interface using WebPa parameter "
					+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Perform Factory reset on the device using WebPA request");
			LOGGER.info("STEP 4 : ACTION : Execute webpa command to factory reset"
					+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET);
			LOGGER.info("STEP 4 : EXPTECTED : WebPA request should return success message");
			LOGGER.info("**********************************************************************************");
			
			status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

			if (status) {
				LOGGER.info("STEP 4: ACTUAL : Device has been factory reset Successfully.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 5 : Verify the Current SSID value for 2.4 Ghz using WebPA request..
			 */
			stepNumber = "s5";
			status = false;
			errorMessage = "Failed to get SSID value for 2.4 Ghz using command "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify the Current SSID value for 2.4 Ghz using WebPA request.");
			LOGGER.info("STEP 5 : ACTION : Execute webpa command:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			LOGGER.info("STEP 5 : EXPTECTED : Current SSID value should be the same as the SSID value from step1");
			LOGGER.info("**********************************************************************************");
			long startTime = System.currentTimeMillis();
			String response = null;
			do {
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
				LOGGER.info("SSID for 2.4 Ghz of the device is " + response);
				status = CommonMethods.isNotNull(response) && response.equals(defaultSSIDName2dot4Ghz);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : Current SSID value(i.e. " + response
						+ ") is same as the SSID value for 2.4Ghz(i.e. " + defaultSSIDName2dot4Ghz + ").");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 6 : Verify the Current SSID value for 5 Ghz using WebPA request.
			 */
			stepNumber = "s6";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify the Current SSID value for 5 Ghz using WebPA request.");
			LOGGER.info("STEP 6 : ACTION : Execute webpa command:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
			LOGGER.info("STEP 6 : EXPTECTED : Current SSID value should be the same as the SSID value from step2");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to get SSID value for 5 Ghz WebPA Parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
			response = null;
			do {
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
				LOGGER.info("SSID for 5 Ghz of the device is " + response);
				status = CommonMethods.isNotNull(response) && response.equals(defaultSSIDName5Ghz);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 6: ACTUAL : Current SSID value(i.e. " + response
						+ ") is same as the SSID value for 5Ghz(i.e. " + defaultSSIDName5Ghz + ").");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING DEFAULT WIFI SSID : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		}

	}

	/**
	 * Test case is created as part of NEW FEATURE AUTOMATION 
	 * 
	 * Test Case # 2: Verify default SSID using SNMP.
	 *
	 * <p>
	 * STEPS:
	 * </p>
	 * <ol>
	 * <li>PRE-CONDITION 1:Reboot the device.</li>
	 * <li>PRE-CONDITION 2:Verify whether SNMP process is Initialized in
	 * Snmplog.txt.0.</li>
	 * <li>S1) Verify retrieving default SSID for 2.4 Ghz using SNMP GET
	 * request.</li>
	 * <li>S2) Verify retrieving default SSID for 5 Ghz using SNMP GET request.</li>
	 * <li>S3) Modify Current SSID name of 2.4 Ghz and 5 Ghz.</li>
	 * <li>S4) Factory reset the device using WebPA set request.</li>
	 * <li>S5) Verify the Current SSID value for 2.4 Ghz using SNMP GET request</li>
	 * <li>S6) Verify the Current SSID value for 5 Ghz using SNMP GET request</li>
	 * </ol>
	 * 
	 * @author Athira
	 * 
	 * @param device {@link Dut}
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
			BroadBandTestGroup.WIFI })
	@TestDetails(testUID = "TC-RDKB-WIFI-1011")
	public void testDefaultSSIDUsingSNMP(Dut device) {
		String testCaseId = "TC-RDKB-WIFI-011";
		int stepNum = 1;
		String stepNumber = "s" + stepNum;
		boolean status = false;
		boolean factoryResetStatus = false;
		String errorMessage = null;
		String defaultSSIDName2Dot4Ghz = null;
		String defaultSSIDName5Ghz = null;
		String response = null;
		long startTime = 0;
		String newSSIDName5Ghz = null;
		int preConStepNumber = 1;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-1011");
			LOGGER.info("TEST DESCRIPTION: Verify default SSID using SNMP");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION 1: Reboot the device");
			LOGGER.info("PRE-CONDITION 2: Verify whether SNMP process is Initialized in Snmplog.txt.0");
			LOGGER.info("1. Verify retrieving default SSID for 2.4 Ghz using SNMP GET request");
			LOGGER.info("2. Verify retrieving default SSID for 5 Ghz using SNMP GET request");
			LOGGER.info("3. Modify Current SSID name of 2.4 Ghz and 5 Ghz");
			LOGGER.info("4. Perform Factory reset on the device using WebPA request");
			LOGGER.info("5. Verify the Current SSID value for 2.4 Ghz using SNMP GET request");
			LOGGER.info("6. Verify the Current SSID value for 5 Ghz using SNMP GET request");
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			/**
			 * PRE-CONDITION 1 : REBOOT THE DEVICE
			 */
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConStepNumber + " : DESCRIPTION : Reboot the device");
			LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTION : Execute command:reboot");
			LOGGER.info(
					"PRE-CONDITION " + preConStepNumber + " : EXPECTED : Reboot of the device should be successful");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to Reboot Device.";
			try {
				status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
			} catch (Exception exception) {
				errorMessage = exception.getMessage();
				LOGGER.error(errorMessage);
			}
			if (status) {
				LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTUAL : Device Rebooted successfully.");
			} else {
				LOGGER.error("PRE-CONDITION " + preConStepNumber + ": ACTUAL : " + errorMessage);
				throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
						+ " : FAILED : " + errorMessage);
			}
			LOGGER.info("#######################################################################################");
			/**
			 * PRE-CONDITION 2 : VERIFY SNMP process is initialized in Snmplog.txt.0
			 */
			preConStepNumber++;
			LOGGER.info("#######################################################################################");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ ": DESCRIPTION : Verify whether SNMP process is Initialized in Snmplog.txt.0.");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : ACTION :Execute:sudo -S stbsshv6 <CMIP> grep -i\"Snmp Initialzed\"/rdklogs/logs/ SNMP.txt.0");
			LOGGER.info("PRE-CONDITION " + preConStepNumber
					+ " : EXPECTED : SNMP process should be Initialized in the Device");
			LOGGER.info("#######################################################################################");
			errorMessage = "SNMP process is not Initialized";
			status = BroadBandSnmpUtils.validateSnmpProcessIsInitialized(tapEnv, device);
			if (status) {
				LOGGER.info("PRE-CONDITION " + preConStepNumber + " : ACTUAL :  SNMP process is Initialized");
			} else {
				LOGGER.error("PRE-CONDITION " + preConStepNumber + " : ACTUAL : " + errorMessage);
				throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION " + preConStepNumber
						+ ": FAILED : " + errorMessage);
			}
			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
			/**
			 * Step 1 : Verify retrieving default SSID for 2.4 Ghz using SNMP GET request.
			 */
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + " : DESCRIPTION : Get Default SSID for 2.4 Ghz using SNMP GET request");
			LOGGER.info("STEP " + stepNum + " : ACTION:Execute snmp command:snmpget -v2c -c <community string> udp6:<ECMIP>"
					+ BroadBandSnmpMib.ECM_DEFAULT_SSID.getOid() + "."
					+ BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID);
			LOGGER.info("STEP " + stepNum + " : EXPECTED : SNMP command must return 2.4Ghz Default SSID value.");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to get default SSID using the SNMP OID " + BroadBandSnmpMib.ECM_DEFAULT_SSID.getOid()
					+ "." + BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID;
			defaultSSIDName2Dot4Ghz = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DEFAULT_SSID.getOid(),
					BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID);
			LOGGER.info("Default SSID for 2.4 Ghz of the device is " + defaultSSIDName2Dot4Ghz);
			status = CommonMethods.isNotNull(defaultSSIDName2Dot4Ghz)
					&& !defaultSSIDName2Dot4Ghz.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE)
					&& !defaultSSIDName2Dot4Ghz.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE)
					&& !CommonMethods.patternMatcher(defaultSSIDName2Dot4Ghz, BroadBandTestConstants.NO_RESPONSE_FROM);
			if (status) {
				LOGGER.info("STEP " + stepNum + ": ACTUAL : Sucessfully Got the default SSID value for the 2.4GHz as: "
						+ defaultSSIDName2Dot4Ghz);
			} else {
				LOGGER.error("STEP " + stepNum + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * Step 2 : Verify retrieving default SSID for 5 Ghz using SNMP GET request.
			 */
			stepNum++;
			stepNumber = "s" + stepNum;
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ " : DESCRIPTION : Verify retrieving default SSID for 5 Ghz using SNMP GET request");
			LOGGER.info("STEP " + stepNum + " : ACTION:Execute snmp command:snmpget -v2c -c <community string>udp6:<ECMIP>"
					+ BroadBandSnmpMib.ECM_DEFAULT_SSID.getOid()+"."
					+ BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID);
			LOGGER.info("STEP " + stepNum + " : EXPECTED : SNMP command must return 5Ghz Default SSID value.");
			LOGGER.info("**********************************************************************************");
			defaultSSIDName5Ghz = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DEFAULT_SSID.getOid(), BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID);
			LOGGER.info("Default SSID for 5 Ghz of the device is " + defaultSSIDName5Ghz);
			errorMessage = "Failed to get default SSID using the SNMP OID " + BroadBandSnmpMib.ECM_DEFAULT_SSID.getOid()
			+ "." + BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID;
			status = CommonMethods.isNotNull(defaultSSIDName5Ghz)
					&& !defaultSSIDName5Ghz.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE)
					&& !defaultSSIDName5Ghz.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE)
					&& !CommonMethods.patternMatcher(defaultSSIDName2Dot4Ghz, BroadBandTestConstants.NO_RESPONSE_FROM);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully Got the default SSID value for the 5GHz as: "
						+ defaultSSIDName5Ghz);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

			/**
			 * Step 3 : Modify Current SSID name of 2.4 Ghz and 5 Ghz.
			 */
			stepNum++;
			stepNumber = "s" + stepNum;
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + " : DESCRIPTION : Modify Current SSID name of 2.4 Ghz and 5 Ghz");
			LOGGER.info("STEP " + stepNum + " : ACTION:Execute webpa set command:"
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
			LOGGER.info("STEP " + stepNum
					+ " : EXPECTED : Current SSID name for 2.4 Ghz and 5 Ghz should be modified with value other than default SSID.");
			LOGGER.info("**********************************************************************************");
			// Set Current 2.4 Ghz SSID name, other than default value SSID
			String newSSIDName2Dot4Ghz = BroadBandTestConstants.STRING_TEST_1.equals(defaultSSIDName2Dot4Ghz)
					? BroadBandTestConstants.STRING_TEST_2
					: BroadBandTestConstants.STRING_TEST_1;
			LOGGER.info("Modify Current SSID name of 2.4 Ghz, other than default SSID value");
			status = BroadBandWiFiUtils.setWebPaParams(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME, newSSIDName2Dot4Ghz,
					BroadBandTestConstants.CONSTANT_0);
			errorMessage = "Failed to set 2.4 Ghz SSID name using the WebPA parameter "
					+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME;
			LOGGER.info("Is Current SSID for 2.4 Ghz modified - " + status);
			startTime = BroadBandTestConstants.CONSTANT_0;
			if (status) {
				// Set Current 5 Ghz SSID name, other than default value SSID
				LOGGER.info("Modify Current SSID name of 5 Ghz, other than default SSID value");
				// "Wifi is busy" error message is thrown, while setting value
				// immediately hence added in loop.
				startTime = System.currentTimeMillis();
				newSSIDName5Ghz = BroadBandTestConstants.STRING_TEST_1.equals(defaultSSIDName2Dot4Ghz)
						? BroadBandTestConstants.STRING_TEST_2
						: BroadBandTestConstants.STRING_TEST_1;
				do {
					status = BroadBandWiFiUtils.setWebPaParams(device,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME, newSSIDName5Ghz,
							BroadBandTestConstants.CONSTANT_0);
				} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
						&& !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
								BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
				errorMessage = "Failed to set 5 Ghz SSID name using the WebPA parameter "
						+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME;
				LOGGER.info("Is Current SSID for 5 Ghz modified - " + status);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Sucessfully modified SSID name for the 2.4GHz as : "
						+ newSSIDName2Dot4Ghz + " and 5GHz as: " + newSSIDName5Ghz);
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * Step 4 : Perform Factory reset on the device using WebPA request.
			 */
			stepNum++;
			stepNumber = "s" + stepNum;
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum + " : DESCRIPTION : Perform Factory reset on the device using WebPA request");
			LOGGER.info("STEP " + stepNum + " : ACTION:Execute webpa command:"
					+ WebPaParamConstants.WEBPA_PARAM_FACTORY_RESET);
			LOGGER.info("STEP " + stepNum + " : EXPECTED : WebPA request should return success message");
			LOGGER.info("**********************************************************************************");

			status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);
			
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Factory reset performed successfully");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}

			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * Step 5 : Verify the Current SSID value for 2.4 Ghz using SNMP GET request.
			 */
			stepNum++;
			stepNumber = "s" + stepNum;
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ " : DESCRIPTION : Verify the Current SSID value for 2.4 Ghz using SNMP GET request");
			LOGGER.info("STEP " + stepNum + " : ACTION:Execute snmp command:snmpget -v2c -c <community string>udp6:<ECMIP>"
					+ BroadBandSnmpMib.ECM_CURRENT_SSID.getOid()
					+ BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID);
			LOGGER.info("STEP " + stepNum
					+ " : EXPECTED : Current SSID value must be the same as the Default SSID value from step1");
			LOGGER.info("**********************************************************************************");
			startTime = System.currentTimeMillis();
			errorMessage = "Failed to verify Current SSID is same as default SSID ";
			do {
				response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device, BroadBandSnmpMib.ECM_CURRENT_SSID.getOid(),
						BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID);
				LOGGER.info("SSID for 2.4 Ghz of the device using SNMP is " + response);
				status = CommonMethods.isNotNull(response) && response.equals(defaultSSIDName2Dot4Ghz);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			errorMessage = "Failed to get SSID using the SNMP OID " + BroadBandSnmpMib.ECM_CURRENT_SSID.getOid()
			+ "." + BroadBandWebPaConstants.WEBPA_INDEX_2_4_GHZ_PRIVATE_SSID;
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Current SSID value is same as default SSID value and is verified successfully");
			} else {

				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
			/**
			 * Step 6 : Verify the Current SSID value for 5 Ghz using SNMP GET request.
			 */
			stepNum++;
			stepNumber = "s" + stepNum;
			status = false;
			errorMessage = BroadBandTestConstants.EMPTY_STRING;
			response = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNum
					+ " : DESCRIPTION : Verify the Current SSID value for 5 Ghz using SNMP GET request");
			LOGGER.info("STEP " + stepNum + " : ACTION:Execute snmp command:snmpget -v2c -c <community string>udp6:<ECMIP>"
					+ BroadBandSnmpMib.ECM_CURRENT_SSID.getOid()
					+ BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID);
			LOGGER.info("STEP " + stepNum
					+ " : EXPECTED : Current SSID value must be the same as the Default SSID value from step2");
			LOGGER.info("**********************************************************************************");
			startTime = System.currentTimeMillis();
			errorMessage = "Failed to verify Current SSID is same as default SSID for 5GHz";
			do {
				response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device, BroadBandSnmpMib.ECM_CURRENT_SSID.getOid(),
						BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID);
				LOGGER.info("SSID for 5 Ghz of the device using SNMP  is " + response);
				status = CommonMethods.isNotNull(response) && response.equals(defaultSSIDName5Ghz);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
			errorMessage = "Failed to get SSID using the SNMP OID " + BroadBandSnmpMib.ECM_CURRENT_SSID.getOid()
			+ "." + BroadBandWebPaConstants.WEBPA_INDEX_5_GHZ_PRIVATE_SSID;
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Current SSID value is same as default SSID value and is verified successfully");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING DEFAULT WIFI SSID : " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		}
	}
	
	  /**
     * Cross-verify the Default Private Wi-Fi SSID and password retrieved from WebPA with the configurations from SNMP
     * <ol>
     * <li>Cross-verify the 2.4 GHz Default Private Wi-Fi SSID retrieved from WebPA with the value retrieved from SNMP
     * </li>
     * <li>Cross-verify the 5 GHz Default Private Wi-Fi SSID retrieved from WebPA with the value retrieved from SNMP
     * </li>
     * <li>Cross-verify the 2.4 GHz Default Private Wi-Fi password retrieved from WebPA with the value retrieved from
     * SNMP</li>
     * <li>Cross-verify the 5 GHz Default Private Wi-Fi password retrieved from WebPA with the value retrieved from SNMP
     * </li>
     * </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-DEF-SSID-5005", testDecription = "Cross-verify the Default Private Wi-Fi SSID and password retrieved from WebPA with the configurations from SNMP")
    public void testToVerifyDefaultPrivateWiFiSsidAndPassword(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-DEF-SSID-505";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-DEF-SSID-5005");
	LOGGER.info(
		"TEST DESCRIPTION: Cross-verify the Default Private Wi-Fi SSID and password retrieved from WebPA with the configurations from SNMP");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Cross-verify the 2.4 GHz Default Private Wi-Fi SSID retrieved from WebPA with the value retrieved from SNMP");
	LOGGER.info(
		"2. Cross-verify the 5 GHz Default Private Wi-Fi SSID retrieved from WebPA with the value retrieved from SNMP");
	LOGGER.info(
		"3. Cross-verify the 2.4 GHz Default Private Wi-Fi password retrieved from WebPA with the value retrieved from SNMP");
	LOGGER.info(
		"4. Cross-verify the 5 GHz Default Private Wi-Fi password retrieved from WebPA with the value retrieved from SNMP");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Cross-verify the 2.4 GHz Default Private Wi-Fi SSID retrieved from WebPA with the value retrieved from SNMP");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the webpa get command on the parameter \"Device.WiFi.SSID.10001.X_COMCAST-COM_DefaultSSID\" and execute the SNMP get command on OID \"1.3.6.1.4.1.17270.50.2.2.2.1.1.16.10001\" and compare these SSIDs retrieved from two different protocols.");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Default Private Wi-Fi SSID of 2.4 GHz retrieved from WebPA should be same as the value obtained from SNMP");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandCommonUtils.executeAndCompareValuesFromWebpaAndSnmp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ,
		    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_SSID_2_4);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Default Private Wi-Fi SSID of 2.4 GHz is successfully retrieved from webPA and cross-verified with the SSID obtained from SNMP , both values are same !");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    status = false;
	    result = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Cross-verify the 5 GHz Default Private Wi-Fi SSID retrieved from WebPA with the value retrieved from SNMP");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the webpa get command on the parameter \"Device.WiFi.SSID.10101.X_COMCAST-COM_DefaultSSID\" and execute the SNMP get command on OID \"1.3.6.1.4.1.17270.50.2.2.2.1.1.16.10101\" and compare these SSIDs retrieved from two different protocols.");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Default Private Wi-Fi SSID of 5 GHz retrieved from WebPA should be same as the value obtained from SNMP");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandCommonUtils.executeAndCompareValuesFromWebpaAndSnmp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ,
		    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_SSID_5);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Default Private Wi-Fi SSID of 5 GHz is successfully retrieved from webPA and cross-verified with the SSID obtained from SNMP , both values are same !");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    status = false;
	    result = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Cross-verify the 2.4 GHz Default Private Wi-Fi password retrieved from WebPA with the value retrieved from SNMP");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the webpa get command on the parameter \"Device.WiFi.AccessPoint.10001.Security.X_COMCAST-COM_DefaultKeyPassphrase\" and execute the SNMP get command on OID \"1.3.6.1.4.1.17270.50.2.2.3.1.1.4.10001\" and compare these passwords retrieved from two different protocols.");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Default Private Wi-Fi password of 2.4 GHz retrieved from WebPA should be same as the value obtained from SNMP");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandCommonUtils.executeAndCompareValuesFromWebpaAndSnmp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_2_4,
		    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_PASSWORD_2_4);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Default Private Wi-Fi password of 2.4 GHz is successfully retrieved from webPA and cross-verified with the SSID obtained from SNMP , both values are same !");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    status = false;
	    result = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Cross-verify the 5 GHz Default Private Wi-Fi password retrieved from WebPA with the value retrieved from SNMP");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the webpa get command on the parameter Device.WiFi.AccessPoint.10101.Security.X_COMCAST-COM_DefaultKeyPassphrase and execute the SNMP get command on OID \"1.3.6.1.4.1.17270.50.2.2.3.1.1.4.10101\" and compare these passwords retrieved from two different protocols.");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Default Private Wi-Fi password of 5 GHz retrieved from WebPA should be same as the value obtained from SNMP");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandCommonUtils.executeAndCompareValuesFromWebpaAndSnmp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_5,
		    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_PASSWORD_5);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Default Private Wi-Fi password of 5 GHz is successfully retrieved from webPA and cross-verified with the SSID obtained from SNMP , both values are same !");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-DEF-SSID-5005");
    }
    
    /**
     * Test to verify data transmission between 2 connected(LAN-WLAN;WLAN to LAN) clients
     * <ol>
     * <li>STEP 1:Retrieve private IPv6 IP's from LAN client and save it</li>
     * <li>STEP 2:Retrieve private IPv6 IP's from WLAN client and save it</li>
     * <li>STEP 3:Ping the IPv6 IP of the WLAN cilent from the LAN client</li>
     * <li>STEP 4:Ping the IPv6 IP of the LAN cilent from the WLAN client</li>
     * </ol>
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WIFI-TRANS-1000")
    public void LanToWifiIPv6DataTransmissionTest(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-WIFI-TRANS-100";// Test case id
	String testStep = null;// Test step number
	String errorMessage = null;// String to store the error message
	Dut ethernetClient = null;// Dut object for ethernet client
	Dut wifiClient = null;// Dut object for wificlient
	String lanIpv6Address = null;// String to store Lan ipv6 address
	String wlanIpv6Address = null;// String to store Wlan ipv6 address

	try {
	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WIFI-TRANS-1000#####################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Test to verify data transmission between 2 connected(LAN-WLAN;WLAN to LAN) clients");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1.Retrieve private IPv6 IP's from LAN client and save it");
	    LOGGER.info("2.Retrieve private IPv6 IP's from WLAN client and save it");
	    LOGGER.info("3.Ping the IPv6  IP of the WLAN cilent from the LAN client ");
	    LOGGER.info("4.Ping the IPv6  IP of the LAN cilent from the WLAN  client ");
	    LOGGER.info("#####################################################################################");
	    /**
	     * STEP 1:Retrieve private IPV6 IP of LAN client and save it .
	     */
	    status = false;
	    testStep = "s1";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 1 : DESCRIPTION :Retrieve private IPV6 IP of LAN client and save it");
	    LOGGER.info("STEP 1 : ACTION:Retrieve IPv6 address of Lan client ");
	    LOGGER.info("STEP 1 : EXPECTED:Private IPv6 address of Lan client should be retrieved successfully");
	    errorMessage = "Unable to retrieve IPv6 address of Lan client";
	    ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    if (null != ethernetClient) {
		lanIpv6Address = BroadBandConnectedClientUtils
			.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(ethernetClient, tapEnv);
		status = CommonMethods.isNotNull(lanIpv6Address);
	    }
	    if (status) {
		LOGGER.info("STEP 1:ACTUAL :Lan client IPv6 address is retrieved successfully;IPv6 Address: "
			+ lanIpv6Address);
	    } else {
		LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 2:Retrieve private IPv6 IP from WLAN client and save it
	     */
	    status = false;
	    testStep = "s2";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 2 : DESCRIPTION :Retrieve private IPv6 IP's from WLAN client and save it");
	    LOGGER.info("STEP 2 : ACTION:Retrieve IPV6 address of Wlan client");
	    LOGGER.info("STEP 2 : EXPECTED:Private IPv6 Address of wlan client should be retrieved successfully");
	    errorMessage = "Unable to retrieve Ipv6 address of wireless client";
	    wifiClient = BroadBandConnectedClientUtils
		    .get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    if (null != wifiClient) {
		wlanIpv6Address = BroadBandConnectedClientUtils
			.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(wifiClient, tapEnv);
		status = CommonMethods.isNotNull(wlanIpv6Address);
	    }
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL :Wlan client IPv6 address retrieved successfully;IPv6 Address: "
			+ wlanIpv6Address);
	    } else {
		LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 3:Ping the IPv6 IP of the WLAN client from the LAN client
	     */
	    status = false;
	    testStep = "s3";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 3 : DESCRIPTION :Ping the IPv6 address of the WLAN cilent from the LAN client");
	    LOGGER.info("STEP 3 : ACTION:Ping WLAN client IPv6 address from LAN client");
	    LOGGER.info(
		    "STEP 3 : EXPECTED:Ping should be successful from LAN client to WLAN client without packet loss");
	    errorMessage = "Unable to ping from LAN client to WLAN client";
	    status = ConnectedNattedClientsUtils.verifyPingConnection(ethernetClient, tapEnv, wlanIpv6Address);
	    if (status) {
		LOGGER.info("STEP 3:ACTUAL :Ping from LAN client to WLAN client is successful");
	    } else {
		LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    LOGGER.info("##########################################################################");

	    /**
	     * STEP 4:Ping the IPv6 IP of the LAN client from the WLAN client
	     */
	    status = false;
	    testStep = "s4";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 4 : DESCRIPTION :Ping the IPv6  address of the LAN cilent from the WLAN  client ");
	    LOGGER.info("STEP 4 : ACTION:Ping LAN client IPv6 address from WLAN client");
	    LOGGER.info(
		    "STEP 4 : EXPECTED:Ping should be successful from WLAN client to LAN client without packet loss");

	    errorMessage = "Unable to ping from WLAN client to LAN client";
	    status = ConnectedNattedClientsUtils.verifyPingConnection(wifiClient, tapEnv, lanIpv6Address);
	    if (status) {
		LOGGER.info("STEP 4:ACTUAL :Ping from WLAN client to LAN client is successful");
	    } else {
		LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	    LOGGER.info("##########################################################################");
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Failed in executing LanToWifiIPv6DataTransmissionTest \n" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Disconnect Wifi Radio 2.4Ghz/5Ghz SSID from the device");
	    LOGGER.info("POST-CONDITION 1 : ACTION :Disconnect wifi radio 2.4Ghz/5Ghz SSID ");
	    LOGGER.info(
		    "POST-CONDITION 1 : EXPECTED : Wifi radio 2.4Ghz/5Ghz SSID should be disconnected successfully");
	    LOGGER.info("#######################################################################################");
	    try {
		BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils
			.disconnectCnnClientFromSsid(tapEnv, device, wifiClient);
		LOGGER.info("POST CONDITION 1:ACTUAL: WIFI SSID 2.4GHZ/5GHZ Disconnect status: "
			+ bandResultObject.isStatus());
	    } catch (Exception exception2) {
		LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 1" + exception2.getMessage());
	    }
	    LOGGER.info("########################### ENDING POST CONFIGURATION ####################################");
	}
	LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WIFI-TRANS-1000#####################");
    }
}
