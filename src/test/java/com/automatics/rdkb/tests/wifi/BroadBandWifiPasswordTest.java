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
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWifiPasswordTest extends AutomaticsTestBase {

    /**
     * Test to verify Encrypt WiFi passwords stored in NVRAM
     * <ol>
     * <li>Verify that wifi passwords are stored in encrypted partition.</li>
     * <li>Verify that the private WiFi SSID name can be read/write using WebPA</li>
     * <li>Verify that the private WiFi SSID name can be read/write using SNMP</li>
     * <li>Verify that the private WiFi passwords can be read/write using WebPA</li>
     * <li>Verify that the private WiFi passwords can be read/write using SNMP</li>
     * <li>Verify that the default WiFi SSID name can be readable using WebPA</li>
     * <li>Verify that the default WiFi SSID name can be readable using SNMP</li>
     * <li>Verify that the default WiFi SSID passwords can be readable using WebPA</li>
     * <li>Verify that the default WiFi passwords can be readable using SNMP</li>
     * <li>Verify backup unencrypted password is removed from /nvram folder</li>
     * </ol>
     * 
     * @author Betel Costrow
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-ENCRYPT-1001")
    public void TestToVerifyEncryptWifiPassword(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-ENCRYPT-001";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	BroadBandResultObject executionResult = new BroadBandResultObject();
	String response = "";
	String wiFiPassword = null;
	int count = BroadBandTestConstants.CONSTANT_0;
	String[] output = null;
	String searchText = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-ENCRYPT-1001");
	LOGGER.info("TEST DESCRIPTION: Test to verify  Encrypt WiFi passwords stored in NVRAM");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify that wifi passwords are stored in encrypted partition.");
	LOGGER.info("2. Verify that the private WiFi SSID name can be read/write using WebPA");
	LOGGER.info("3. Verify that the private WiFi SSID name can be read/write using SNMP");
	LOGGER.info("4. Verify that the private WiFi passwords can be read/write using WebPA");
	LOGGER.info("5. Verify that the private WiFi passwords can be read/write using SNMP");
	LOGGER.info("6. Verify that the default WiFi SSID name can be readable using WebPA");
	LOGGER.info("7. Verify that the default WiFi SSID name can be readable using SNMP");
	LOGGER.info("8. Verify that the default WiFi SSID passwords can be readable using WebPA");
	LOGGER.info("9. Verify that the default WiFi passwords can be readable using SNMP");
	LOGGER.info("10. Verify backup unencrypted password is removed from /nvram folder");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify that wifi passwords are stored in encrypted partition.");
	    LOGGER.info(
		    "STEP 1: ACTION :  1)Check if the location obtained from propertyfile based on device Model is ecrypted");
	    LOGGER.info("STEP 1: EXPECTED : the file containing the password should present encrypted partition");

	    String FileLocation = BroadbandPropertyFileHandler.getEncryptedFileLocationBasedOnDeviceModel(device);
	    if (CommonMethods.isNotNull(FileLocation)) {

		errorMessage = FileLocation + " is not encrypted";

		status = BroadBandCommonUtils.isFileEncrypted(tapEnv, device, FileLocation, false);
		if (!status) {
		    status = CommonMethods.isFileExists(device, tapEnv, FileLocation);
		}
	    } else {
		status = false;
		errorMessage = "device not supported";

	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Successfully verified Wifi passwords are present under encrypted partition.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Private Wifi SSID name is not read and writeable using WEBPA";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify that the private WiFi SSID name can be read/write using WebPA");
	    LOGGER.info("STEP 2: ACTION : 1)Device.WiFi.SSID.1.SSID 2)Device.WiFi.SSID.2.SSID ");
	    LOGGER.info("STEP 2: EXPECTED : Private Wifi name should be read and writeable using WEBPA");
	    LOGGER.info("**********************************************************************************");

	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.TWO_GHZ_SSID_NAME_CHANGE)) {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
			BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.FIVE_GHZ_SSID_NAME_CHANGE);
	    }

	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Successfully verified private WiFi SSID name can be read/writeable using WebPA");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Private Wifi SSID name is not read and writeable using SNMP";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify that the private WiFi SSID name can be read/write using SNMP");
	    LOGGER.info("STEP 3: ACTION : 1)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>"
		    + "2)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>");
	    LOGGER.info("STEP 3: EXPECTED : Private Wifi name should be read and writeable using SNMP");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isRunningEthwanMode()) {
		LOGGER.info("This Step is not applicable for Ethwan Mode");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			"Step is not applicable for ETHWAN devices", false);
	    } else {
		executionResult = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(),
			BroadBandTestConstants.TWO_GHZ_SSID_NAME_CHANGE, SnmpDataType.STRING,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex());
		if (executionResult.isStatus()) {
		    executionResult = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid(),
			    BroadBandTestConstants.FIVE_GHZ_SSID_NAME_CHANGE, SnmpDataType.STRING,
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex());
		    status = executionResult.isStatus();
		}
		errorMessage = errorMessage + executionResult.getErrorMessage();

		if (status) {
		    LOGGER.info(
			    "STEP 3: ACTUAL : Successfully verified private WiFi SSID name can be read/writeable using SNMP");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Private Wifi password is not read and writeable using WEBPA";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify that the private WiFi passwords can be read/write using WebPA");
	    LOGGER.info(
		    "STEP 4: ACTION : 1)Device.WiFi.AccessPoint.1.Security.KeyPassphrase 2)Device.WiFi.AccessPoint.2.Security.KeyPassphrase");
	    LOGGER.info("STEP 4: EXPECTED : private wifi password should be read and writeable using WEBPA");
	    LOGGER.info("**********************************************************************************");

	    if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_2GHZ_PASSPHRASE,
		    BroadBandTestConstants.CONSTANT_0, BroadbandPropertyFileHandler.getPrivateWifiPassPhrase())) {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_PRIVATE_SSID_5GHZ_PASSPHRASE,
			BroadBandTestConstants.CONSTANT_0, BroadbandPropertyFileHandler.getPrivateWifiPassPhrase());
	    }

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Successfully verified private WiFi passwords name can be read/writeable using WebPA");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Private Wifi password is not read and writeable using SNMP";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify that the private WiFi passwords can be read/write using SNMP");
	    LOGGER.info("STEP 5: ACTION : 1)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>"
		    + "2)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>");
	    LOGGER.info("STEP 5: EXPECTED : private wifi password should be read and writeable using SNMP");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isRunningEthwanMode()) {
		LOGGER.info("This Step is not applicable for Ethwan Mode");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			"Step is not applicable for ETHWAN devices", false);
	    } else {
		executionResult = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getOid(),
			BroadbandPropertyFileHandler.getPrivateWifiPassPhrase(), SnmpDataType.STRING,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_2_4_PASSPHRASE.getTableIndex());
		if (executionResult.isStatus()) {
		    executionResult = BroadBandSnmpUtils.snmpSetAndVerifySNMPResponseRdkDevices(device, tapEnv,
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getOid(),
			    BroadbandPropertyFileHandler.getPrivateWifiPassPhrase(), SnmpDataType.STRING,
			    BroadBandSnmpMib.ECM_PRIVATE_WIFI_5_PASSPHRASE.getTableIndex());
		    status = executionResult.isStatus();
		}
		errorMessage = errorMessage + executionResult.getErrorMessage();

		if (status) {
		    LOGGER.info(
			    "STEP 5: ACTUAL : Successfully verified private WiFi passwords name can be read/writeable using SNMP");
		} else {
		    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Default wifi SSID name is not readable using WEBPA";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify that the default WiFi SSID name can be readable using WebPA");
	    LOGGER.info(
		    "STEP 6: ACTION : 1)Device.WiFi.SSID.1.X_COMCAST-COM_DefaultSSID 2)Device.WiFi.SSID.2.X_COMCAST-COM_DefaultSSID");
	    LOGGER.info("STEP 6: EXPECTED : Default wifi SSID name should be readable using WEBPA");
	    LOGGER.info("**********************************************************************************");

	    String defaultSSIDName = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_2_4_GHZ);
	    if (CommonMethods.isNotNull(defaultSSIDName)) {
		defaultSSIDName = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_DEFAULT_SSID_NAME_5_GHZ);
		status = CommonMethods.isNotNull(defaultSSIDName);
	    }

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Successfully verified default WiFi SSID name can be readable using WebPA");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Default wifi SSID name is not readable using SNMP";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify that the default WiFi SSID name can be readable using SNMP");
	    LOGGER.info("STEP 7: ACTION : 1)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>"
		    + "2)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>");
	    LOGGER.info("STEP 7: EXPECTED : Default wifi SSID name should be readable using SNMP");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isRunningEthwanMode()) {
		LOGGER.info("This Step is not applicable for Ethwan Mode");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			"Step is not applicable for ETHWAN devices", false);
	    } else {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_SSID_2_4.getOid(),
			BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_SSID_2_4.getTableIndex());
		if (CommonMethods.isNotNull(response)) {
		    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_SSID_5.getOid(),
			    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_SSID_5.getTableIndex());
		}
		status = CommonMethods.isNotNull(response);

		if (status) {
		    LOGGER.info(
			    "STEP 7: ACTUAL : Successfully verified default WiFi SSID name can be readable using SNMP");
		} else {
		    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Default wifi password is not readable using WEBPA";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify that the default WiFi passwords can be readable using WebPA");
	    LOGGER.info(
		    "STEP 8: ACTION : 1)Device.WiFi.AccessPoint.1.Security.X_COMCAST-COM_DefaultKeyPassphrase 2)Device.WiFi.AccessPoint.2.Security.X_COMCAST-COM_DefaultKeyPassphrase");
	    LOGGER.info("STEP 8: EXPECTED : Default wifi password should be readable using WEBPA");
	    LOGGER.info("**********************************************************************************");

	    String defaultSSIDPassword = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_2_4);
	    if (CommonMethods.isNotNull(defaultSSIDPassword)) {
		defaultSSIDPassword = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_PASSWORD_DEFAULT_SSID_5);
		status = CommonMethods.isNotNull(defaultSSIDPassword);
	    }

	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Successfully verified default WiFi passwords can be readable using WEBPA");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Default wifi password is not readable using SNMP";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify that the default WiFi passwords can be readable using SNMP");
	    LOGGER.info("STEP 9: ACTION : 1)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>"
		    + "2)snmp* -v2c -c <COMMUNITY STRING> udp6:<ipv6_address> <MIBOid>");
	    LOGGER.info("STEP 9: EXPECTED : Default wifi password should be readable using SNMP");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isRunningEthwanMode()) {
		LOGGER.info("This Step is not applicable for Ethwan Mode");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			"Step is not applicable for ETHWAN devices", false);
	    } else {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_PASSWORD_2_4.getOid(),
			BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_PASSWORD_2_4.getTableIndex());
		if (CommonMethods.isNotNull(response)) {
		    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_PASSWORD_5.getOid(),
			    BroadBandSnmpMib.DEFAULT_PRIVATE_WIFI_PASSWORD_5.getTableIndex());
		}
		status = CommonMethods.isNotNull(response);

		if (status) {
		    LOGGER.info(
			    "STEP 9: ACTUAL : Successfully verified default WiFi passwords can be readable using SNMP");
		} else {
		    LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "backup unencrypted password is present in /nvram folder";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify backup unencrypted password is removed from /nvram folder");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute grep command: grep -rinI <PasswordName> /nvram/ | grep -v /nvram/secure/ ");
	    LOGGER.info("STEP 10: EXPECTED : Response should be null (OR) No Such File Or Directory");
	    LOGGER.info("**********************************************************************************");

	    wiFiPassword = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
	    searchText = BroadBandCommandConstants.CMD_WIFI_PASSWORD_NVRAM
		    .replace(BroadBandTestConstants.STRING_REPLACE, wiFiPassword);
	    if (CommonMethods.isNotNull(wiFiPassword)) {
		response = tapEnv.executeCommandUsingSsh(device, searchText);
		status = CommonMethods.isNull(response);
	    }
	    if (!status) {
		output = response.split(BroadBandTestConstants.CHAR_NEW_LINE);
		for (int i = BroadBandTestConstants.CONSTANT_0; i < output.length; i++) {
		    if (!CommonMethods.isGivenStringAvailableInCommandOutput(output[i],
			    BroadBandTraceConstants.LOG_MESSAGE_GREP_NO_SUCH_FILE_OR_DIRECTORY)) {
			break;
		    } else {
			count = count + BroadBandTestConstants.CONSTANT_1;
		    }
		}
		status = output.length == count;
	    }
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : Successfully verified unencrypted password is not present in /nvram folder");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (

	Exception e) {
	    e.printStackTrace();
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-ENCRYPT-1001");
    }
}
